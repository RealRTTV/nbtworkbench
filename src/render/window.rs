use std::borrow::Cow;
use std::ops::DerefMut;
use std::sync::Arc;
use std::time::Duration;

use serde::{Deserialize, Serialize};
#[cfg(target_arch = "wasm32")] use wasm_bindgen::JsValue;
use wgpu::util::{BufferInitDescriptor, DeviceExt};
use wgpu::*;
use winit::application::ApplicationHandler;
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::event::*;
use winit::event_loop::{ActiveEventLoop, EventLoop};
#[cfg(target_arch = "wasm32")] use winit::platform::web::WindowExtWebSys;
#[cfg(target_os = "windows")]
use winit::platform::windows::WindowAttributesExtWindows;
use winit::window::{Icon, Window, WindowAttributes, WindowId};
use zune_inflate::DeflateOptions;

use crate::action_result::ActionResult;
use crate::config::get_theme;
use crate::render::VertexBufferBuilder;
use crate::render::assets::{ATLAS_HEIGHT, ATLAS_WIDTH, HEADER_SIZE, ICON_HEIGHT, ICON_WIDTH, UNICODE_LEN, atlas, icon};
use crate::render::widget::alert::manager::Alertable;
use crate::render::widget::search_box::{SEARCH_BOX_END_X, SEARCH_BOX_START_X};
use crate::util::Timestamp;
use crate::workbench::Workbench;
use crate::{WORKBENCH, error, window_properties};

pub const WINDOW_HEIGHT: u32 = 420;
pub const WINDOW_WIDTH: u32 = 720;
pub const MIN_WINDOW_HEIGHT: u32 = HEADER_SIZE as u32 + 64;
pub const MIN_WINDOW_WIDTH: u32 = 480;

#[allow(static_mut_refs)]
pub async fn run() -> ! {
	struct Handler<'window> {
		state: State<'window>,
		workbench: &'static mut Workbench,
		window: Arc<Window>,
	}

	impl<'window> ApplicationHandler<()> for Handler<'window> {
		fn resumed(&mut self, _: &ActiveEventLoop) {}
		fn window_event(&mut self, _: &ActiveEventLoop, window_id: WindowId, event: WindowEvent) {
			if self.window.id() != window_id {
				return;
			}

			#[cfg(target_arch = "wasm32")]
			crate::wasm::on_input();
			if window_properties().should_ignore_events() {
				return;
			}
			if State::input(event.clone(), self.workbench) == ActionResult::Pass {
				match event {
					WindowEvent::RedrawRequested => match self.state.render(self.workbench, self.window.as_ref()) {
						Ok(()) => {}
						Err(SurfaceError::Lost | SurfaceError::Outdated) => self.state.surface.configure(&self.state.device, &self.state.config),
						Err(SurfaceError::OutOfMemory) => std::process::exit(1),
						Err(SurfaceError::Timeout) => {
							error!("Frame took too long to process")
						}
						Err(SurfaceError::Other) => {
							error!("Failed to acquire texture")
						}
					},
					WindowEvent::CloseRequested =>
						if self.workbench.close() == 0 {
							std::process::exit(0)
						},
					WindowEvent::Resized(new_size) => {
						self.state.resize(self.workbench, new_size);
						// self.window.request_redraw();
					}
					_ => {}
				}
			}
		}

		fn about_to_wait(&mut self, _: &ActiveEventLoop) {
			#[cfg(target_arch = "wasm32")]
			{
				let old_size = self.window.inner_size();
				let scaling_factor = web_sys::window().map_or(1.0, |window| window.device_pixel_ratio());
				let new_size: PhysicalSize<u32> = web_sys::window()
					.map(|window| {
						PhysicalSize::new(
							(window.inner_width().ok().as_ref().and_then(JsValue::as_f64).expect("Width must exist") * scaling_factor).ceil() as u32,
							(window.inner_height().ok().as_ref().and_then(JsValue::as_f64).expect("Height must exist") * scaling_factor).ceil() as u32,
						)
					})
					.expect("Window has dimension properties");
				if new_size != old_size {
					let _ = self.window.request_inner_size(new_size);
					self.state.resize(self.workbench, new_size);
				}
			}
			self.window.request_redraw();
		}
	}

	let event_loop = EventLoop::builder().build().expect("Event loop was unconstructable");
	#[allow(unused_mut)]
	let mut builder = WindowAttributes::default()
		.with_title("NBT Workbench")
		.with_inner_size(PhysicalSize::new(7680, 4320))
		.with_min_inner_size(PhysicalSize::new(MIN_WINDOW_WIDTH as u32, MIN_WINDOW_HEIGHT as u32))
		.with_window_icon(Some(Icon::from_rgba(icon(), ICON_WIDTH as u32, ICON_HEIGHT as u32).expect("valid format")));
	#[cfg(target_os = "windows")]
	{
		builder = builder.with_drag_and_drop(true);
	}
	let window = Arc::new(event_loop.create_window(builder).expect("Unable to construct window"));
	#[cfg(target_arch = "wasm32")]
	let window_size = {
		web_sys::window()
			.and_then(|window| {
				let document = window.document()?;
				let width = window.inner_width().ok()?.as_f64()?;
				let height = window.inner_height().ok()?.as_f64()?;
				Some((document, PhysicalSize::new(width as u32, height as u32)))
			})
			.and_then(|(document, size)| {
				let canvas = web_sys::HtmlElement::from(window.canvas()?);
				canvas.set_id("canvas");
				document.body()?.append_child(&canvas).ok()?;
				let _ = window.request_inner_size(size);
				let _ = canvas.focus();
				Some(size)
			})
			.expect("Couldn't append canvas to document body")
	};
	#[cfg(not(target_arch = "wasm32"))]
	let window_size = {
		let (window_width_pct, window_height_pct) = (WINDOW_WIDTH as f64 / 1920.0, WINDOW_HEIGHT as f64 / 1080.0);
		let monitor_dims = window.current_monitor().map(|monitor| monitor.size()).unwrap_or(PhysicalSize::new(1920, 1080));
		let width = (f64::round(window_width_pct * monitor_dims.width as f64) as u32).max(MIN_WINDOW_WIDTH as u32);
		let height = (f64::round(window_height_pct * monitor_dims.height as f64) as u32).max(MIN_WINDOW_HEIGHT as u32);
		let size = PhysicalSize::new(width, height);
		let _ = window.request_inner_size(size);
		size
	};
	let state = State::new(&window, window_size).await;
	*window_properties().deref_mut() = WindowProperties::new(Arc::clone(&window));
	unsafe {
		std::ptr::write(&raw mut WORKBENCH, Workbench::new(Some(window_size)).expect("Valid workbench construction"));
	}
	let mut handler = unsafe {
		Handler {
			state,
			workbench: &mut WORKBENCH,
			window: Arc::clone(&window),
		}
	};
	event_loop.run_app(&mut handler).expect("Event loop failed");
	loop {}
}

#[derive(Copy, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Theme {
	Light,
	#[default]
	Dark,
}

impl From<winit::window::Theme> for Theme {
	fn from(value: winit::window::Theme) -> Self {
		match value {
			winit::window::Theme::Light => Self::Light,
			winit::window::Theme::Dark => Self::Dark,
		}
	}
}

pub struct State<'window> {
	surface: Surface<'window>,
	device: Device,
	queue: Queue,
	config: SurfaceConfiguration,
	render_pipeline: RenderPipeline,
	size: PhysicalSize<u32>,
	diffuse_texture_bind_group: BindGroup,
	diffuse_texture: Texture,
	text_render_pipeline: RenderPipeline,
	unicode_bind_group: BindGroup,
	last_tick: Timestamp,
	previous_theme: Theme,
}

impl<'window> State<'window> {
	#[allow(clippy::too_many_lines)] // yeah, but... what am I supposed to do?
	async fn new(window: &'window Window, size: PhysicalSize<u32>) -> State<'window> {
		let instance = Instance::new(&InstanceDescriptor {
			backends: Instance::enabled_backend_features(),
			flags: if cfg!(debug_assertions) { InstanceFlags::advanced_debugging() } else { InstanceFlags::empty() },
			backend_options: BackendOptions {
				gl: GlBackendOptions {
					gles_minor_version: Gles3MinorVersion::default(),
					fence_behavior: GlFenceBehavior::default(),
				},
				dx12: Dx12BackendOptions { shader_compiler: Dx12Compiler::Fxc },
				noop: NoopBackendOptions { enable: false },
			},
		});
		let surface = instance.create_surface(window).expect("Surface was able to be created");
		let adapter = instance
			.request_adapter(&RequestAdapterOptions {
				power_preference: PowerPreference::None,
				force_fallback_adapter: false,
				compatible_surface: Some(&surface),
			})
			.await
			.expect("Could not obtain adapter");
		let (device, queue) = adapter
			.request_device(&DeviceDescriptor {
				required_features: adapter.features(),
				required_limits: if cfg!(target_arch = "wasm32") {
					Limits {
						max_texture_dimension_2d: 4096,
						..Limits::downlevel_webgl2_defaults()
					}
				} else {
					Limits {
						max_compute_workgroups_per_dimension: 0,
						..Limits::default()
					}
				},
				label: None,
				memory_hints: Default::default(),
				trace: Trace::Off,
			})
			.await
			.expect("Could obtain device");
		let format = surface.get_capabilities(&adapter).formats.into_iter().find(|format| !format.is_srgb()).expect("An SRGB format exists");
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
		let diffuse_texture_size = Extent3d {
			width: ATLAS_WIDTH as u32,
			height: ATLAS_HEIGHT as u32,
			depth_or_array_layers: 1,
		};
		let diffuse_texture = device.create_texture(&TextureDescriptor {
			size: diffuse_texture_size,
			mip_level_count: 1,
			sample_count: 1,
			dimension: TextureDimension::D2,
			format: TextureFormat::Rgba8UnormSrgb,
			usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
			label: Some("Diffuse Texture"),
			view_formats: &[],
		});
		queue.write_texture(
			TexelCopyTextureInfo {
				texture: &diffuse_texture,
				mip_level: 0,
				origin: Origin3d::ZERO,
				aspect: TextureAspect::All,
			},
			atlas(get_theme()),
			TexelCopyBufferLayout {
				offset: 0,
				bytes_per_row: Some(4 * ATLAS_WIDTH as u32),
				rows_per_image: Some(ATLAS_HEIGHT as u32),
			},
			diffuse_texture_size,
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
		let diffuse_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
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
		let diffuse_texture_bind_group = device.create_bind_group(&BindGroupDescriptor {
			layout: &diffuse_bind_group_layout,
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
			source: ShaderSource::Wgsl(Cow::Borrowed(crate::render::shader::SOURCE)),
		});
		let render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
			label: Some("Render Pipeline Layout"),
			bind_group_layouts: &[&diffuse_bind_group_layout],
			push_constant_ranges: &[],
		});
		let render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
			label: Some("Render Pipeline"),
			layout: Some(&render_pipeline_layout),
			vertex: VertexState {
				module: &shader,
				entry_point: Some("vertex"),
				compilation_options: Default::default(),
				buffers: &[VertexBufferLayout {
					array_stride: 20,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x3, 1 => Float32x2],
				}],
			},
			fragment: Some(FragmentState {
				module: &shader,
				entry_point: Some("fragment"),
				compilation_options: Default::default(),
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
			cache: None,
		});
		let unicode_texture = device.create_texture(&TextureDescriptor {
			label: Some("Unicode Texture Array"),
			size: Extent3d {
				width: 512,
				height: UNICODE_LEN as u32 / 512,
				depth_or_array_layers: 1,
			},
			mip_level_count: 1,
			sample_count: 1,
			dimension: TextureDimension::D2,
			format: TextureFormat::R8Unorm,
			usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
			view_formats: &[],
		});
		queue.write_texture(
			unicode_texture.as_image_copy(),
			&{
				zune_inflate::DeflateDecoder::new_with_options(include_bytes!("../assets/unicode.hex.zib"), DeflateOptions::default().set_confirm_checksum(false))
					.decode_zlib()
					.ok()
					.expect("there is no way this fails, otherwise i deserve the ub that comes from this.")
			},
			TexelCopyBufferLayout {
				offset: 0,
				bytes_per_row: Some(512),
				rows_per_image: Some(UNICODE_LEN as u32 / 512),
			},
			Extent3d {
				width: 512,
				height: UNICODE_LEN as u32 / 512,
				depth_or_array_layers: 1,
			},
		);
		let unicode_texture_view = unicode_texture.create_view(&TextureViewDescriptor::default());
		let unicode_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
			label: Some("Unicode Bind Group Layout"),
			entries: &[BindGroupLayoutEntry {
				binding: 0,
				visibility: ShaderStages::FRAGMENT,
				ty: BindingType::Texture {
					sample_type: TextureSampleType::Float { filterable: false },
					view_dimension: TextureViewDimension::D2,
					multisampled: false,
				},
				count: None,
			}],
		});
		let unicode_bind_group = device.create_bind_group(&BindGroupDescriptor {
			label: Some("Unicode Bind Group"),
			layout: &unicode_bind_group_layout,
			entries: &[BindGroupEntry {
				binding: 0,
				resource: BindingResource::TextureView(&unicode_texture_view),
			}],
		});
		let text_shader = device.create_shader_module(ShaderModuleDescriptor {
			label: Some("Text Shader"),
			source: ShaderSource::Wgsl(Cow::Borrowed(crate::render::text_shader::SOURCE)),
		});
		let text_render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
			label: Some("Text Render Pipeline Layout"),
			bind_group_layouts: &[&unicode_bind_group_layout],
			push_constant_ranges: &[],
		});
		let text_render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
			label: Some("Text Render Pipeline"),
			layout: Some(&text_render_pipeline_layout),
			vertex: VertexState {
				module: &text_shader,
				entry_point: Some("vertex"),
				compilation_options: Default::default(),
				buffers: &[VertexBufferLayout {
					array_stride: 16,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x2, 1 => Uint32, 2 => Uint32],
				}],
			},
			fragment: Some(FragmentState {
				module: &text_shader,
				entry_point: Some("fragment"),
				compilation_options: Default::default(),
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
			cache: None,
		});

		Self {
			surface,
			device,
			queue,
			config,
			render_pipeline,
			size,
			diffuse_texture_bind_group,
			diffuse_texture,
			text_render_pipeline,
			unicode_bind_group,
			last_tick: Timestamp::UNIX_EPOCH,
			previous_theme: get_theme(),
		}
	}

	fn get_backdrop_color(&self) -> Color {
		match get_theme() {
			Theme::Light => Color {
				r: 0xFB as f64 / 255.0,
				g: 0xF8 as f64 / 255.0,
				b: 0xF4 as f64 / 255.0,
				a: 1.0,
			},
			Theme::Dark => Color {
				r: 0.11774103726,
				g: 0.11774103726,
				b: 0.11774103726,
				a: 1.0,
			},
		}
	}

	fn resize(&mut self, workbench: &mut Workbench, new_size: PhysicalSize<u32>) {
		if new_size.width > 0 && new_size.height > 0 {
			self.size = new_size;
			self.config.width = new_size.width;
			self.config.height = new_size.height;
			self.surface.configure(&self.device, &self.config);
			workbench.on_window_dims(new_size);
			for tab in &mut workbench.tabs {
				tab.refresh_scrolls();
			}
		}
	}

	fn input(event: WindowEvent, workbench: &mut Workbench) -> ActionResult {
		match event {
			WindowEvent::DroppedFile(file) if let Some(data) = std::fs::read(&file).alert_err(&mut workbench.alerts) => {
				workbench.on_open_file(&file, &data).alert_err(&mut workbench.alerts);
				ActionResult::Success(())
			}
			WindowEvent::KeyboardInput { event, .. } => workbench.on_key_input(event),
			WindowEvent::CursorMoved { position, .. } => workbench.on_mouse_move(position),
			WindowEvent::CursorLeft { .. } => workbench.on_mouse_move(PhysicalPosition::new(0.0, 0.0)),
			WindowEvent::MouseWheel { delta, .. } => workbench.on_scroll(delta),
			WindowEvent::MouseInput { state, button, .. } => workbench.on_mouse_input(state, button),
			WindowEvent::Touch(touch) => match touch.phase {
				TouchPhase::Started => {
					workbench.on_mouse_move(touch.location)?;
					workbench.on_mouse_input(ElementState::Pressed, MouseButton::Left)
				}
				TouchPhase::Moved => workbench.on_mouse_move(touch.location),
				TouchPhase::Ended | TouchPhase::Cancelled => {
					workbench.on_mouse_move(touch.location)?;
					workbench.on_mouse_input(ElementState::Released, MouseButton::Left)
				}
			},
			_ => ActionResult::Pass,
		}
	}

	fn render(&mut self, workbench: &mut Workbench, window: &Window) -> Result<(), SurfaceError> {
		if self.last_tick.elapsed() >= Duration::from_millis(25) {
			self.last_tick = Timestamp::now();
			workbench.tick();
		}

		if self.previous_theme != get_theme() {
			self.queue.write_texture(
				TexelCopyTextureInfo {
					texture: &self.diffuse_texture,
					mip_level: 0,
					origin: Origin3d::ZERO,
					aspect: TextureAspect::All,
				},
				atlas(get_theme()),
				TexelCopyBufferLayout {
					offset: 0,
					bytes_per_row: Some(4 * ATLAS_WIDTH as u32),
					rows_per_image: Some(ATLAS_HEIGHT as u32),
				},
				Extent3d {
					width: ATLAS_WIDTH as u32,
					height: ATLAS_HEIGHT as u32,
					depth_or_array_layers: 1,
				},
			);
		}

		self.previous_theme = get_theme();
		let surface_texture = self.surface.get_current_texture()?;
		let view = surface_texture.texture.create_view(&TextureViewDescriptor::default());
		let size = Extent3d {
			width: surface_texture.texture.width(),
			height: surface_texture.texture.height(),
			depth_or_array_layers: 1,
		};
		let mut encoder = self.device.create_command_encoder(&CommandEncoderDescriptor { label: Some("Command Encoder") });
		let depth_texture = self.device.create_texture(&TextureDescriptor {
			label: Some("Depth Texture"),
			size,
			mip_level_count: 1,
			sample_count: 1,
			dimension: TextureDimension::D2,
			format: TextureFormat::Depth32Float,
			usage: TextureUsages::RENDER_ATTACHMENT | TextureUsages::TEXTURE_BINDING,
			view_formats: &[],
		});
		let depth_texture_view = depth_texture.create_view(&TextureViewDescriptor::default());

		let mut builder = VertexBufferBuilder::new(self.size, workbench.tabs.active_tab().consts().scroll, workbench.scale);

		{
			let vertex_buffer;
			let index_buffer;
			let text_vertex_buffer;
			let text_index_buffer;

			let search_boxes_vertex_buffer;
			let search_boxes_index_buffer;
			let search_boxes_text_vertex_buffer;
			let search_boxes_text_index_buffer;

			let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
				label: Some("Render Pass"),
				color_attachments: &[Some(RenderPassColorAttachment {
					view: &view,
					resolve_target: None,
					ops: Operations {
						load: LoadOp::Clear(self.get_backdrop_color()),
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
			// let start = std::time::Instant::now();
			workbench.render(&mut builder);
			// println!("Full Frame: {}ms", start.elapsed().as_millis_f64());

			let show_cursor = true;
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
				render_pass.set_bind_group(0, &self.diffuse_texture_bind_group, &[]);

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

			render_pass.set_scissor_rect(
				(SEARCH_BOX_START_X as f32 * workbench.scale).floor() as u32,
				(22.0 * workbench.scale).floor() as u32,
				((workbench.window_dims.width as usize - SEARCH_BOX_END_X - SEARCH_BOX_START_X) as f32 * workbench.scale).ceil() as u32,
				(46.0 * workbench.scale).ceil() as u32,
			);
			builder.reset();
			workbench.render_search_boxes(&mut builder);

			{
				render_pass.set_pipeline(&self.text_render_pipeline);
				render_pass.set_bind_group(0, &self.unicode_bind_group, &[]);

				search_boxes_text_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Text Vertex Buffer"),
					contents: builder.text_vertices(),
					usage: BufferUsages::VERTEX,
				});

				search_boxes_text_index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Text Index Buffer"),
					contents: builder.text_indices(),
					usage: BufferUsages::INDEX,
				});

				render_pass.set_vertex_buffer(0, search_boxes_text_vertex_buffer.slice(..));
				render_pass.set_index_buffer(search_boxes_text_index_buffer.slice(..), IndexFormat::Uint32);

				render_pass.draw_indexed(0..builder.text_indices_len(), 0, 0..1);
			}

			{
				render_pass.set_pipeline(&self.render_pipeline);
				render_pass.set_bind_group(0, &self.diffuse_texture_bind_group, &[]);

				search_boxes_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Vertex Buffer"),
					contents: builder.vertices(),
					usage: BufferUsages::VERTEX,
				});

				search_boxes_index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
					label: Some("Index Buffer"),
					contents: builder.indices(),
					usage: BufferUsages::INDEX,
				});

				render_pass.set_vertex_buffer(0, search_boxes_vertex_buffer.slice(..));
				render_pass.set_index_buffer(search_boxes_index_buffer.slice(..), IndexFormat::Uint16);

				render_pass.draw_indexed(0..builder.indices_len(), 0, 0..1);
			}
		}

		window.pre_present_notify();

		self.queue.submit(Some(encoder.finish()));
		surface_texture.present();
		Ok(())
	}
}

pub enum WindowProperties {
	Real { window: Arc<Window>, ignore_event_end: Timestamp },
	Fake,
}

impl WindowProperties {
	pub const fn new(window: Arc<Window>) -> Self {
		Self::Real {
			window,
			ignore_event_end: Timestamp::UNIX_EPOCH,
		}
	}

	pub fn set_window_title(&self, title: &str) {
		if let Self::Real { window, .. } = self {
			window.set_title(title);
			#[cfg(target_arch = "wasm32")]
			if let Some(document) = web_sys::window().and_then(|window| window.document()) {
				let _ = document.set_title(title);
			}
		}
	}

	pub fn ignore_events_for(&mut self, duration: Duration) {
		if let Self::Real { ignore_event_end, .. } = self {
			*ignore_event_end = Timestamp::now() + duration;
		}
	}

	#[must_use]
	pub fn should_ignore_events(&self) -> bool { if let Self::Real { ignore_event_end, .. } = self { Timestamp::now() < *ignore_event_end } else { false } }
}
