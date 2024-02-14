use std::borrow::Cow;
use std::num::NonZeroU64;
use std::time::Duration;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::JsValue;

use wgpu::util::{BufferInitDescriptor, DeviceExt};
#[allow(clippy::wildcard_imports)]
use wgpu::*;
use winit::dpi::PhysicalSize;
#[allow(clippy::wildcard_imports)]
use winit::event::*;
use winit::event_loop::EventLoop;
#[cfg(target_os = "windows")]
use winit::platform::windows::WindowBuilderExtWindows;
#[cfg(target_arch = "wasm32")]
use winit::platform::web::WindowExtWebSys;
use winit::window::{Icon, Window, WindowBuilder};
use zune_inflate::DeflateOptions;

use crate::alert::Alert;
use crate::assets::HEADER_SIZE;
use crate::color::TextColor;
use crate::vertex_buffer_builder::VertexBufferBuilder;
use crate::workbench::Workbench;
use crate::{assets, log, error, OptionExt, since_epoch, WindowProperties};

pub const WINDOW_HEIGHT: usize = 420;
pub const WINDOW_WIDTH: usize = 620;
pub const MIN_WINDOW_HEIGHT: usize = HEADER_SIZE + 16;
pub const MIN_WINDOW_WIDTH: usize = 520;

pub async fn run() -> ! {
	let event_loop = EventLoop::new().expect("Event loop was unconstructable");
	let mut builder = WindowBuilder::new()
		.with_title("NBT Workbench")
		.with_inner_size(PhysicalSize::new(WINDOW_WIDTH as u32, WINDOW_HEIGHT as u32))
		.with_min_inner_size(PhysicalSize::new(
			MIN_WINDOW_WIDTH as u32,
			MIN_WINDOW_HEIGHT as u32,
		))
		.with_window_icon(Some(
			Icon::from_rgba(
				assets::icon(),
				assets::ICON_WIDTH as u32,
				assets::ICON_HEIGHT as u32,
			)
			.expect("valid format"),
		));
	#[cfg(not(target_arch = "wasm32"))] {
		builder = builder
			.with_drag_and_drop(true)
			.with_transparent(std::env::args().any(|x| x.eq("--transparent")));
	}
	let window = builder.build(&event_loop).expect("Window was constructable");
	#[cfg(target_arch = "wasm32")] {
		web_sys::window().and_then(|window| {
			let document = window.document()?;
			let width = window.inner_width().ok()?.as_f64()?;
			let height = window.inner_height().ok()?.as_f64()?;
			Some((document, PhysicalSize::new(width as u32, height as u32)))
		}).and_then(|(document, size)| {
			let canvas = web_sys::Element::from(window.canvas()?);
			document.body()?.append_child(&canvas).ok()?;
			let _ = window.request_inner_size(size);
			Some(())
		}).expect("Couldn't append canvas to document body")
	}
	let mut state = State::new(&window).await;
	let mut window_properties = WindowProperties::new(&window);
	let mut workbench = Workbench::new(&mut window_properties);
	event_loop.run(|event, _| match event {
		Event::WindowEvent { event, window_id } if window_id == window.id() => {
			if !State::input(&event, &mut workbench, &window) {
				match event {
					WindowEvent::RedrawRequested => {
						match state.render(&mut workbench, &window) {
							Ok(()) => {}
							Err(SurfaceError::Lost) => state.surface.configure(&state.device, &state.config),
							Err(SurfaceError::OutOfMemory) => std::process::exit(1),
							Err(SurfaceError::Timeout) => error!("Frame took too long to process"),
							Err(SurfaceError::Outdated) => {
								error!("Surface changed unexpectedly");
								std::process::exit(1);
							}
						}
					}
					WindowEvent::CloseRequested => std::process::exit(0),
					WindowEvent::Resized(new_size) => state.resize(&mut workbench, new_size),
					_ => {}
				}
			}
		}
		Event::AboutToWait => {
			#[cfg(target_arch = "wasm32")] {
				let old_size = window.inner_size();
				let new_size: PhysicalSize<u32> = web_sys::window().map(|window| PhysicalSize::new(window.inner_width().ok().as_ref().and_then(JsValue::as_f64).expect("Width must exist") as u32, window.inner_height().ok().as_ref().and_then(JsValue::as_f64).expect("Height must exist") as u32)).expect("Window has dimension properties");
				if new_size != old_size {
					let _ = window.request_inner_size(new_size);
					state.resize(&mut workbench, new_size);
				}
			}

			window.request_redraw();
		}
		_ => {}
	}).expect("Event loop failed");
	loop {}
}

pub struct State<'window> {
	surface: Surface<'window>,
	device: Device,
	queue: Queue,
	config: SurfaceConfiguration,
	render_pipeline: RenderPipeline,
	size: PhysicalSize<u32>,
	diffuse_bind_group: BindGroup,
	text_render_pipeline: RenderPipeline,
	unicode_bind_group: BindGroup,
	load_op: LoadOp<Color>,
	last_tick: Duration,
}

impl<'window> State<'window> {
	#[allow(clippy::too_many_lines)] // yeah, but.... what am I supposed to do?
	async fn new(window: &'window Window) -> Self {
		let size = window.inner_size();
		let instance = Instance::new(InstanceDescriptor {
			backends: Backends::all(),
			flags: Default::default(),
			dx12_shader_compiler: Dx12Compiler::default(),
			gles_minor_version: Default::default(),
		});
		let surface = unsafe { instance.create_surface(window).ok().panic_unchecked("all safety guarantees are assured") };
		let adapter = instance
			.request_adapter(&RequestAdapterOptions {
				power_preference: PowerPreference::None,
				force_fallback_adapter: false,
				compatible_surface: Some(&surface),
			})
			.await
			.expect("Could not obtain adapter");
		let (device, queue) = adapter
			.request_device(
				&DeviceDescriptor {
					required_features: adapter.features(),
					required_limits: if cfg!(target_arch = "wasm32") {
						Limits {
							max_storage_buffers_per_shader_stage: 1,
							..Limits::downlevel_webgl2_defaults()
						}
					} else {
						Limits::default()
					},
					label: None,
				},
				None,
			)
			.await
			.expect("Could obtain device");
		let format = dbg!(surface
			.get_capabilities(&adapter)
			.formats)
			.into_iter()
			.find(|format| !format.is_srgb())
			.expect("An SRGB format exists");
		let config = SurfaceConfiguration {
			usage: TextureUsages::RENDER_ATTACHMENT,
			format,
			width: size.width,
			height: size.height,
			present_mode: PresentMode::AutoVsync,
			desired_maximum_frame_latency: 0,
			alpha_mode: CompositeAlphaMode::Auto,
			view_formats: vec![],
		};
		surface.configure(&device, &config);
		let texture_size = Extent3d {
			width: assets::ATLAS_WIDTH as u32,
			height: assets::ATLAS_HEIGHT as u32,
			depth_or_array_layers: 1,
		};
		let diffuse_texture = device.create_texture(&TextureDescriptor {
			size: texture_size,
			mip_level_count: 1,
			sample_count: 1,
			dimension: TextureDimension::D2,
			format: TextureFormat::Rgba8UnormSrgb,
			usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
			label: Some("Diffuse Texture"),
			view_formats: &[],
		});
		queue.write_texture(
			ImageCopyTexture {
				texture: &diffuse_texture,
				mip_level: 0,
				origin: Origin3d::ZERO,
				aspect: TextureAspect::All,
			},
			assets::ATLAS,
			ImageDataLayout {
				offset: 0,
				bytes_per_row: Some(4 * assets::ATLAS_WIDTH as u32),
				rows_per_image: Some(assets::ATLAS_HEIGHT as u32),
			},
			texture_size,
		);
		let diffuse_texture_view = diffuse_texture.create_view(&TextureViewDescriptor::default());
		let diffuse_sampler = device.create_sampler(&SamplerDescriptor {
			label: Some("Diffuse Sampler"),
			address_mode_u: AddressMode::Repeat,
			address_mode_v: AddressMode::Repeat,
			address_mode_w: AddressMode::Repeat,
			mag_filter: FilterMode::Nearest,
			min_filter: FilterMode::Nearest,
			mipmap_filter: FilterMode::Nearest,
			..Default::default()
		});
		let texture_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
			entries: &[
				BindGroupLayoutEntry {
					binding: 0,
					visibility: ShaderStages::FRAGMENT,
					ty: BindingType::Texture {
						multisampled: false,
						view_dimension: TextureViewDimension::D2,
						sample_type: TextureSampleType::Float { filterable: true },
					},
					count: None,
				},
				BindGroupLayoutEntry {
					binding: 1,
					visibility: ShaderStages::FRAGMENT,
					ty: BindingType::Sampler(SamplerBindingType::Filtering),
					count: None,
				},
			],
			label: Some("Texture Bind Group Layout"),
		});
		let diffuse_bind_group = device.create_bind_group(&BindGroupDescriptor {
			layout: &texture_bind_group_layout,
			entries: &[
				BindGroupEntry {
					binding: 0,
					resource: BindingResource::TextureView(&diffuse_texture_view),
				},
				BindGroupEntry {
					binding: 1,
					resource: BindingResource::Sampler(&diffuse_sampler),
				},
			],
			label: Some("Diffuse Bind Group"),
		});
		let shader = device.create_shader_module(ShaderModuleDescriptor {
			label: Some("Shader"),
			source: ShaderSource::Wgsl(Cow::Borrowed(crate::shader::SOURCE)),
		});
		let render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
			label: Some("Render Pipeline Layout"),
			bind_group_layouts: &[&texture_bind_group_layout],
			push_constant_ranges: &[],
		});
		let render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
			label: Some("Render Pipeline"),
			layout: Some(&render_pipeline_layout),
			vertex: VertexState {
				module: &shader,
				entry_point: "v",
				buffers: &[VertexBufferLayout {
					array_stride: 20,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x3, 1 => Float32x2],
				}],
			},
			fragment: Some(FragmentState {
				module: &shader,
				entry_point: "f",
				targets: &[Some(ColorTargetState {
					format: config.format,
					blend: Some(BlendState::ALPHA_BLENDING),
					write_mask: ColorWrites::ALL,
				})],
			}),
			primitive: PrimitiveState {
				topology: PrimitiveTopology::TriangleList,
				strip_index_format: None,
				front_face: FrontFace::Ccw,
				cull_mode: Some(Face::Back),
				polygon_mode: PolygonMode::Fill,
				unclipped_depth: false,
				conservative: false,
			},
			depth_stencil: Some(DepthStencilState {
				format: TextureFormat::Depth32Float,
				depth_write_enabled: true,
				depth_compare: CompareFunction::LessEqual,
				stencil: StencilState::default(),
				bias: DepthBiasState::default(),
			}),
			multisample: MultisampleState {
				count: 1,
				mask: !0,
				alpha_to_coverage_enabled: false,
			},
			multiview: None,
		});
		let unicode_buffer = device.create_buffer_init(&BufferInitDescriptor {
			label: Some("Unicode Buffer"),
			contents: &{
				// ~5ms with --release
				#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
				let start = std::time::Instant::now();
				let vec = unsafe {
					zune_inflate::DeflateDecoder::new_with_options(
						include_bytes!("assets/unicode.hex.zib"),
						DeflateOptions::default().set_confirm_checksum(false),
					)
					.decode_zlib()
					.ok()
					.panic_unchecked("there is no way this fails, otherwise i deserve the ub that comes from this.")
				};
				#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
				log!(
					"took {}ms to read compressed unicode",
					start.elapsed().as_nanos() as f64 / 1_000_000.0
				);
				vec
			},
			usage: BufferUsages::STORAGE,
		});
		let unicode_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
			label: Some("Unicode Bind Group Layout"),
			entries: &[BindGroupLayoutEntry {
				binding: 0,
				visibility: ShaderStages::FRAGMENT,
				ty: BindingType::Buffer {
					ty: BufferBindingType::Storage { read_only: true },
					has_dynamic_offset: false,
					min_binding_size: NonZeroU64::new(assets::UNICODE_LEN as u64),
				},
				count: None,
			}],
		});
		let unicode_bind_group = device.create_bind_group(&BindGroupDescriptor {
			label: Some("Unicode Bind Group"),
			layout: &unicode_bind_group_layout,
			entries: &[BindGroupEntry {
				binding: 0,
				resource: BindingResource::Buffer(BufferBinding {
					buffer: &unicode_buffer,
					offset: 0,
					size: None,
				}),
			}],
		});
		let text_shader = device.create_shader_module(ShaderModuleDescriptor {
			label: Some("Text Shader"),
			source: ShaderSource::Wgsl(Cow::Borrowed(crate::text_shader::SOURCE)),
		});
		let text_render_pipeline_payout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
			label: Some("Text Render Pipeline Layout"),
			bind_group_layouts: &[&unicode_bind_group_layout],
			push_constant_ranges: &[],
		});
		let text_render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
			label: Some("Text Render Pipeline"),
			layout: Some(&text_render_pipeline_payout),
			vertex: VertexState {
				module: &text_shader,
				entry_point: "vt",
				buffers: &[VertexBufferLayout {
					array_stride: 16,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x2, 1 => Uint32, 2 => Uint32],
				}],
			},
			fragment: Some(FragmentState {
				module: &text_shader,
				entry_point: "ft",
				targets: &[Some(ColorTargetState {
					format: config.format,
					blend: Some(BlendState::ALPHA_BLENDING),
					write_mask: ColorWrites::ALL,
				})],
			}),
			primitive: PrimitiveState {
				topology: PrimitiveTopology::TriangleList,
				strip_index_format: None,
				front_face: FrontFace::Ccw,
				cull_mode: Some(Face::Back),
				polygon_mode: PolygonMode::Fill,
				unclipped_depth: false,
				conservative: false,
			},
			depth_stencil: Some(DepthStencilState {
				format: TextureFormat::Depth32Float,
				depth_write_enabled: true,
				depth_compare: CompareFunction::LessEqual,
				stencil: StencilState::default(),
				bias: DepthBiasState::default(),
			}),
			multisample: MultisampleState {
				count: 1,
				mask: !0,
				alpha_to_coverage_enabled: false,
			},
			multiview: None,
		});
		let load_op = if std::env::args().any(|x| x.eq("--transparent")) {
			LoadOp::Load
		} else {
			LoadOp::Clear(Color {
				r: 0.11774103726,
				g: 0.11774103726,
				b: 0.11774103726,
				a: 1.0,
			})
		};

		Self {
			surface,
			device,
			queue,
			config,
			render_pipeline,
			size,
			diffuse_bind_group,
			text_render_pipeline,
			unicode_bind_group,
			load_op,
			last_tick: Duration::ZERO,
		}
	}

	fn resize(&mut self, workbench: &mut Workbench, new_size: PhysicalSize<u32>) {
		if new_size.width > 0 && new_size.height > 0 {
			self.size = new_size;
			self.config.width = new_size.width;
			self.config.height = new_size.height;
			self.surface.configure(&self.device, &self.config);
			workbench.window_dimensions(new_size.width as usize, new_size.height as usize);
			for tab in &mut workbench.tabs {
				tab.scroll = tab.scroll();
				tab.horizontal_scroll = tab.horizontal_scroll(workbench.held_entry.element());
			}
		}
	}

	#[allow(clippy::match_same_arms)] // it's cool to be reminded more often how many things I can have events for to make nbt workbench more reactive
	fn input(event: &WindowEvent, workbench: &mut Workbench, window: &Window) -> bool {
		match event {
			WindowEvent::Resized(size) => {
				workbench.window_height = size.height as usize;
				for entry in &mut workbench.tabs {
					entry.window_height = workbench.window_height;
					entry.scroll = entry.scroll();
				}
				false
			}
			WindowEvent::Moved(_) => false,
			WindowEvent::CloseRequested => false,
			WindowEvent::Destroyed => false,
			WindowEvent::DroppedFile(file) => {
				if let Err(e) = workbench.on_open_file(file, &mut WindowProperties::new(&window)) {
					workbench.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
				}
				true
			}
			WindowEvent::HoveredFile(_) => false,
			WindowEvent::HoveredFileCancelled => false,
			WindowEvent::Focused(_) => false,
			WindowEvent::KeyboardInput { event, .. } => workbench.on_key_input(event, &mut WindowProperties::new(&window)),
			WindowEvent::ModifiersChanged(_) => false,
			WindowEvent::CursorMoved { position, .. } => workbench.on_mouse_move(*position),
			WindowEvent::CursorEntered { .. } => false,
			WindowEvent::CursorLeft { .. } => false,
			WindowEvent::MouseWheel { delta, .. } => workbench.on_scroll(*delta),
			WindowEvent::MouseInput { state, button, .. } => workbench.on_mouse_input(*state, *button, &mut WindowProperties::new(&window)),
			WindowEvent::TouchpadPressure { .. } => false,
			WindowEvent::AxisMotion { .. } => false,
			WindowEvent::Touch(_) => false,
			WindowEvent::ScaleFactorChanged { .. } => false,
			WindowEvent::ThemeChanged(_) => false,
			WindowEvent::Ime(_) => false,
			WindowEvent::Occluded(_) => false,
			WindowEvent::TouchpadMagnify { .. } => false,
			WindowEvent::SmartMagnify { .. } => false,
			WindowEvent::TouchpadRotate { .. } => false,
			WindowEvent::ActivationTokenDone { .. } => false,
			WindowEvent::RedrawRequested => false,
		}
	}

	fn render(&mut self, workbench: &mut Workbench, window: &Window) -> Result<(), SurfaceError> {
		if (since_epoch() - self.last_tick).as_millis() >= 25
		{
			workbench.tick();
			self.last_tick = since_epoch();
		}
		if let Err(e) = workbench.try_subscription() {
			workbench.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
		}
		let output = self.surface.get_current_texture()?;
		let view = output
			.texture
			.create_view(&TextureViewDescriptor::default());
		let mut encoder = self
			.device
			.create_command_encoder(&CommandEncoderDescriptor {
				label: Some("Command Encoder"),
			});
		let depth_texture = self.device.create_texture(&TextureDescriptor {
			label: Some("Depth Texture"),
			size: Extent3d {
				width: self.config.width,
				height: self.config.height,
				depth_or_array_layers: 1,
			},
			mip_level_count: 1,
			sample_count: 1,
			dimension: TextureDimension::D2,
			format: TextureFormat::Depth32Float,
			usage: TextureUsages::RENDER_ATTACHMENT | TextureUsages::TEXTURE_BINDING,
			view_formats: &[],
		});
		let depth_texture_view = depth_texture.create_view(&TextureViewDescriptor::default());

		{
			let vertex_buffer;
			let index_buffer;
			let text_vertex_buffer;
			let text_index_buffer;
			let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
				label: Some("Render Pass"),
				color_attachments: &[Some(RenderPassColorAttachment {
					view: &view,
					resolve_target: None,
					ops: Operations {
						load: self.load_op,
						store: StoreOp::Store,
					},
				})],
				depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
					view: &depth_texture_view,
					depth_ops: Some(Operations {
						load: LoadOp::Clear(1.0),
						store: StoreOp::Store,
					}),
					stencil_ops: None,
				}),
				timestamp_writes: None,
				occlusion_query_set: None,
			});

			let mut builder = VertexBufferBuilder::new(
				self.size,
				assets::ATLAS_WIDTH,
				assets::ATLAS_HEIGHT,
				workbench.scroll(),
				workbench.scale,
			);
			workbench.render(&mut builder);

			let show_cursor = !builder.drew_tooltip();
			if show_cursor != workbench.cursor_visible {
				window.set_cursor_visible(show_cursor);
				workbench.cursor_visible = show_cursor;
			}

			{
				render_pass.set_pipeline(&self.text_render_pipeline);
				render_pass.set_bind_group(0, &self.unicode_bind_group, &[]);

				text_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Text Vertex Buffer"),
					contents: builder.text_vertices(),
					usage: BufferUsages::VERTEX,
				});

				text_index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Text Index Buffer"),
					contents: builder.text_indices(),
					usage: BufferUsages::INDEX,
				});

				render_pass.set_vertex_buffer(0, text_vertex_buffer.slice(..));
				render_pass.set_index_buffer(text_index_buffer.slice(..), IndexFormat::Uint32);

				render_pass.draw_indexed(0..builder.text_indices_len(), 0, 0..1);
			}

			{
				render_pass.set_pipeline(&self.render_pipeline);
				render_pass.set_bind_group(0, &self.diffuse_bind_group, &[]);

				vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Vertex Buffer"),
					contents: builder.vertices(),
					usage: BufferUsages::VERTEX,
				});

				index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Index Buffer"),
					contents: builder.indices(),
					usage: BufferUsages::INDEX,
				});

				render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
				render_pass.set_index_buffer(index_buffer.slice(..), IndexFormat::Uint16);

				render_pass.draw_indexed(0..builder.indices_len(), 0, 0..1);
			}
		}

		self.queue.submit(std::iter::once(encoder.finish()));
		output.present();

		Ok(())
	}
}
