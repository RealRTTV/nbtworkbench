use std::borrow::Cow;
use std::cell::UnsafeCell;
use std::rc::Rc;
use std::time::Duration;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::JsValue;

use wgpu::util::{BufferInitDescriptor, DeviceExt};
#[allow(clippy::wildcard_imports)]
use wgpu::*;
use winit::dpi::{PhysicalPosition, PhysicalSize};
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
use crate::{assets, WORKBENCH, WINDOW_PROPERTIES, error, OptionExt, since_epoch, WindowProperties};

pub const WINDOW_HEIGHT: usize = 420;
pub const WINDOW_WIDTH: usize = 720;
pub const MIN_WINDOW_HEIGHT: usize = HEADER_SIZE + 16;
pub const MIN_WINDOW_WIDTH: usize = 720;

pub async fn run() -> ! {
	let event_loop = EventLoop::new().expect("Event loop was unconstructable");
	let builder = WindowBuilder::new()
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
	let window = Rc::new('a: {
		#[cfg(target_os = "windows")] {
			break 'a builder.with_drag_and_drop(true)
		}
		#[cfg(not(target_os = "windows"))]
		break 'a builder
	}.build(&event_loop).expect("Window was constructable"));
	#[cfg(target_arch = "wasm32")]
	let window_size = {
		web_sys::window().and_then(|window| {
			let document = window.document()?;
			let width = window.inner_width().ok()?.as_f64()?;
			let height = window.inner_height().ok()?.as_f64()?;
			Some((document, PhysicalSize::new(width as u32, height as u32)))
		}).and_then(|(document, size)| {
			let canvas = web_sys::HtmlElement::from(window.canvas()?);
			canvas.set_id("canvas");
			document.body()?.append_child(&canvas).ok()?;
			let _ = window.request_inner_size(size);
			let _ = canvas.focus();
			Some(size)
		}).expect("Couldn't append canvas to document body")
	};
	#[cfg(not(target_arch = "wasm32"))]
	let window_size = PhysicalSize::new(WINDOW_WIDTH as u32, WINDOW_HEIGHT as u32);
	let mut state = State::new(&window, window_size).await;
	unsafe { std::ptr::write(std::ptr::addr_of_mut!(WINDOW_PROPERTIES), UnsafeCell::new(WindowProperties::new(Rc::clone(&window)))); }
	let window_properties = unsafe { WINDOW_PROPERTIES.get_mut() };
	unsafe { std::ptr::write(std::ptr::addr_of_mut!(WORKBENCH), UnsafeCell::new(Workbench::new(window_properties))); }
	let workbench = unsafe { WORKBENCH.get_mut() };
	event_loop.run(|event, _| match event {
		Event::WindowEvent { event, window_id } if window_id == window.id() => {
			#[cfg(target_arch = "wasm32")]
			crate::on_input();
			if !State::input(&event, workbench, window_properties) {
				match event {
					WindowEvent::RedrawRequested => {
						match state.render(workbench, window.as_ref()) {
							Ok(()) => {}
							Err(SurfaceError::Lost | SurfaceError::Outdated) => state.surface.configure(&state.device, &state.config),
							Err(SurfaceError::OutOfMemory) => std::process::exit(1),
							Err(SurfaceError::Timeout) => { error!("Frame took too long to process") },
						}
					}
					WindowEvent::CloseRequested => if workbench.close() == 0 { std::process::exit(0) },
					WindowEvent::Resized(new_size) => state.resize(workbench, new_size),
					_ => {}
				}
			}
		}
		Event::AboutToWait => {
			#[cfg(target_arch = "wasm32")] {
				let old_size = window.inner_size();
				let scaling_factor = web_sys::window().map_or(1.0, |window| window.device_pixel_ratio());
				let new_size: PhysicalSize<u32> = web_sys::window().map(|window| PhysicalSize::new((window.inner_width().ok().as_ref().and_then(JsValue::as_f64).expect("Width must exist") * scaling_factor).ceil() as u32, (window.inner_height().ok().as_ref().and_then(JsValue::as_f64).expect("Height must exist") * scaling_factor).ceil() as u32)).expect("Window has dimension properties");
				if new_size != old_size {
					let _ = window.request_inner_size(new_size);
					state.resize(workbench, new_size);
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
	tooltip_effect_render_pipeline: RenderPipeline,
	texture_reference_bind_group_layout: BindGroupLayout,
	sampler: Sampler,
	last_tick: Duration,
	copy_render_pipeline: RenderPipeline,
}

impl<'window> State<'window> {
	#[allow(clippy::too_many_lines)] // yeah, but... what am I supposed to do?
	async fn new(window: &'window Window, size: PhysicalSize<u32>) -> State<'window> {
		let instance = Instance::new(InstanceDescriptor {
			backends: Backends::all(),
			flags: Default::default(),
			dx12_shader_compiler: Dx12Compiler::default(),
			gles_minor_version: Default::default(),
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
			.request_device(
				&DeviceDescriptor {
					required_features: adapter.features(),
					required_limits: if cfg!(target_arch = "wasm32") {
						Limits::downlevel_webgl2_defaults()
					} else {
						Limits::default()
					},
					label: None,
				},
				None,
			)
			.await
			.expect("Could obtain device");
		let format = surface
			.get_capabilities(&adapter)
			.formats
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
			assets::atlas(),
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
				entry_point: "vertex",
				buffers: &[VertexBufferLayout {
					array_stride: 20,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x3, 1 => Float32x2],
				}],
			},
			fragment: Some(FragmentState {
				module: &shader,
				entry_point: "fragment",
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
		let unicode_texture = device.create_texture(&TextureDescriptor {
			label: Some("Unicode Texture Array"),
			size: Extent3d {
				width: 512,
				height: assets::UNICODE_LEN as u32 / 512,
				depth_or_array_layers: 1,
			},
			mip_level_count: 1,
			sample_count: 1,
			dimension: TextureDimension::D2,
			format: TextureFormat::R8Unorm,
			usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
			view_formats: &[],
		});
		queue.write_texture(unicode_texture.as_image_copy(), &unsafe {
			zune_inflate::DeflateDecoder::new_with_options(
				include_bytes!("assets/unicode.hex.zib"),
				DeflateOptions::default().set_confirm_checksum(false),
			).decode_zlib().ok().panic_unchecked("there is no way this fails, otherwise i deserve the ub that comes from this.")
		}, ImageDataLayout {
			offset: 0,
			bytes_per_row: Some(512),
			rows_per_image: Some(assets::UNICODE_LEN as u32 / 512),
		}, Extent3d {
			width: 512,
			height: assets::UNICODE_LEN as u32 / 512,
			depth_or_array_layers: 1,
		});
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
			source: ShaderSource::Wgsl(Cow::Borrowed(crate::text_shader::SOURCE)),
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
				entry_point: "vertex",
				buffers: &[VertexBufferLayout {
					array_stride: 16,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x2, 1 => Uint32, 2 => Uint32],
				}],
			},
			fragment: Some(FragmentState {
				module: &text_shader,
				entry_point: "fragment",
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
		let texture_reference_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
			label: Some("Tooltip Effect Bind Group Layout"),
			entries: &[
				BindGroupLayoutEntry {
					binding: 0,
					visibility: ShaderStages::FRAGMENT,
					ty: BindingType::Texture {
						sample_type: TextureSampleType::Float {
							filterable: true,
						},
						view_dimension: TextureViewDimension::D2,
						multisampled: false,
					},
					count: None,
				},
				BindGroupLayoutEntry {
					binding: 1,
					visibility: ShaderStages::FRAGMENT,
					ty: BindingType::Sampler(SamplerBindingType::Filtering),
					count: None,
				}
			],
		});
		let tooltip_effect_shader = device.create_shader_module(ShaderModuleDescriptor {
			label: Some("Tooltip Effect Shader"),
			source: ShaderSource::Wgsl(Cow::Borrowed(crate::tooltip_effect_shader::SOURCE))
		});
		let tooltip_effect_render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
			label: Some("Tooltip Effect Render Pipeline Layout"),
			bind_group_layouts: &[&texture_reference_bind_group_layout],
			push_constant_ranges: &[],
		});
		let tooltip_effect_render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
			label: Some("Tooltip Effect Render Pipeline"),
			layout: Some(&tooltip_effect_render_pipeline_layout),
			vertex: VertexState {
				module: &tooltip_effect_shader,
				entry_point: "vertex",
				buffers: &[VertexBufferLayout {
					array_stride: 8,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x2],
				}],
			},
			fragment: Some(FragmentState {
				module: &tooltip_effect_shader,
				entry_point: "fragment",
				targets: &[Some(ColorTargetState {
					format: config.format,
					blend: Some(BlendState::REPLACE),
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
			depth_stencil: None,
			multisample: MultisampleState {
				count: 1,
				mask: !0,
				alpha_to_coverage_enabled: false,
			},
			multiview: None,
		});
		let copy_render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
			label: Some("Copy Render Pipeline Layout"),
			bind_group_layouts: &[&texture_reference_bind_group_layout],
			push_constant_ranges: &[],
		});
		let copy_shader = device.create_shader_module(ShaderModuleDescriptor {
			label: Some("Copy Shader"),
			source: ShaderSource::Wgsl(Cow::Borrowed(crate::copy_shader::SOURCE))
		});
		let copy_render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
			label: Some("Copy Render Pipeline"),
			layout: Some(&copy_render_pipeline_layout),
			vertex: VertexState {
				module: &copy_shader,
				entry_point: "vertex",
				buffers: &[VertexBufferLayout {
					array_stride: 8,
					step_mode: VertexStepMode::Vertex,
					attributes: &vertex_attr_array![0 => Float32x2],
				}],
			},
			primitive: PrimitiveState {
				topology: PrimitiveTopology::TriangleList,
				strip_index_format: None,
				front_face: FrontFace::Ccw,
				cull_mode: Some(Face::Back),
				unclipped_depth: false,
				polygon_mode: PolygonMode::Fill,
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
			fragment: Some(FragmentState {
				module: &copy_shader,
				entry_point: "fragment",
				targets: &[Some(ColorTargetState {
					format: config.format,
					blend: Some(BlendState::ALPHA_BLENDING),
					write_mask: ColorWrites::ALL,
				})],
			}),
			multiview: None,
		});
		let sampler = device.create_sampler(&SamplerDescriptor {
			label: Some("Sampler"),
			address_mode_u: AddressMode::Repeat,
			address_mode_v: AddressMode::Repeat,
			address_mode_w: AddressMode::Repeat,
			mag_filter: FilterMode::Nearest,
			min_filter: FilterMode::Nearest,
			mipmap_filter: FilterMode::Nearest,
			..Default::default()
		});

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
			tooltip_effect_render_pipeline,
			texture_reference_bind_group_layout,
			copy_render_pipeline,
			sampler,
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
	fn input(event: &WindowEvent, workbench: &mut Workbench, window_properties: &mut WindowProperties) -> bool {
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
				if let Err(e) = workbench.on_open_file(file, std::fs::read(file).unwrap_or(vec![]), window_properties) {
					workbench.alert(Alert::new("Error!", TextColor::Red, e.to_string()))
				}
				true
			}
			WindowEvent::HoveredFile(_) => false,
			WindowEvent::HoveredFileCancelled => false,
			WindowEvent::Focused(_) => false,
			WindowEvent::KeyboardInput { event, .. } => workbench.on_key_input(event, window_properties),
			WindowEvent::ModifiersChanged(_) => false,
			WindowEvent::CursorMoved { position, .. } => workbench.on_mouse_move(*position),
			WindowEvent::CursorEntered { .. } => false,
			WindowEvent::CursorLeft { .. } => workbench.on_mouse_move(PhysicalPosition::new(0.0, 0.0)),
			WindowEvent::MouseWheel { delta, .. } => workbench.on_scroll(*delta),
			WindowEvent::MouseInput { state, button, .. } => workbench.on_mouse_input(*state, *button, window_properties),
			WindowEvent::TouchpadPressure { .. } => false,
			WindowEvent::AxisMotion { .. } => false,
			WindowEvent::Touch(touch) => match touch.phase {
				TouchPhase::Started => {
					workbench.on_mouse_move(touch.location);
					workbench.on_mouse_input(ElementState::Pressed, MouseButton::Left, window_properties)
				}
				TouchPhase::Moved => workbench.on_mouse_move(touch.location),
				TouchPhase::Ended | TouchPhase::Cancelled => {
					workbench.on_mouse_move(touch.location);
					workbench.on_mouse_input(ElementState::Released, MouseButton::Left, window_properties)
				}
			},
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
		let surface_texture = self.surface.get_current_texture()?;
		let size = Extent3d {
			width: surface_texture.texture.width(),
			height: surface_texture.texture.height(),
			depth_or_array_layers: 1,
		};
		let surface_view = surface_texture.texture.create_view(&TextureViewDescriptor::default());
		let texture = self.device.create_texture(&TextureDescriptor {
			label: Some("Texture"),
			size,
			mip_level_count: 1,
			sample_count: 1,
			dimension: TextureDimension::D2,
			format: surface_texture.texture.format(),
			usage: TextureUsages::RENDER_ATTACHMENT | TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_SRC,
			view_formats: &[],
		});
		let view = texture.create_view(&TextureViewDescriptor::default());
		let mut encoder = self
			.device
			.create_command_encoder(&CommandEncoderDescriptor {
				label: Some("Command Encoder"),
			});
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

		let mut builder = VertexBufferBuilder::new(
			self.size,
			assets::ATLAS_WIDTH,
			assets::ATLAS_HEIGHT,
			workbench.scroll(),
			workbench.scale,
		);

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
						load: LoadOp::Clear(Color {
							r: 0.11774103726,
							g: 0.11774103726,
							b: 0.11774103726,
							a: 1.0,
						}),
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
			workbench.render(&mut builder);

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

		self.queue.submit(Some(encoder.finish()));

		if builder.drew_tooltip() {
			builder.clear_buffers();
			let mut encoder = self.device.create_command_encoder(&CommandEncoderDescriptor { label: Some("Tooltip Command Encoder"), });

			let tooltip_effect_texture = self.device.create_texture(&TextureDescriptor {
				label: Some("Tooltip Effect Texture"),
				size,
				mip_level_count: 1,
				sample_count: 1,
				dimension: TextureDimension::D2,
				format: surface_texture.texture.format(),
				usage: TextureUsages::RENDER_ATTACHMENT | TextureUsages::TEXTURE_BINDING,
				view_formats: &[],
			});

			let tooltip_effect_texture_view = tooltip_effect_texture.create_view(&TextureViewDescriptor::default());

			let vertices = {
				let tooltip_effect_vertex_buffer;
				let tooltip_effect_bind_group;

				let mut tooltip_effect_render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
					label: Some("Tooltip Effect Render Pass"),
					color_attachments: &[Some(RenderPassColorAttachment {
						view: &tooltip_effect_texture_view,
						resolve_target: None,
						ops: Operations {
							load: LoadOp::Clear(Color {
								r: 0.0,
								g: 0.0,
								b: 0.0,
								a: 0.0,
							}),
							store: StoreOp::Store,
						},
					})],
					depth_stencil_attachment: None,
					timestamp_writes: None,
					occlusion_query_set: None,
				});

				if let Some(vertices) = builder.draw_tooltip0() {
					tooltip_effect_bind_group = self.device.create_bind_group(&BindGroupDescriptor {
						label: Some("Tooltip Effect Bind Group"),
						layout: &self.texture_reference_bind_group_layout,
						entries: &[
							BindGroupEntry {
								binding: 0,
								resource: BindingResource::TextureView(&view),
							},
							BindGroupEntry {
								binding: 1,
								resource: BindingResource::Sampler(&self.sampler),
							}
						],
					});

					tooltip_effect_render_pass.set_pipeline(&self.tooltip_effect_render_pipeline);
					tooltip_effect_render_pass.set_bind_group(0, &tooltip_effect_bind_group, &[]);

					tooltip_effect_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
						label: Some("Tooltip Effect Vertex Buffer"),
						contents: unsafe { std::slice::from_raw_parts(vertices.as_ptr().cast::<u8>(), vertices.len() * 4) },
						usage: BufferUsages::VERTEX,
					});

					tooltip_effect_render_pass.set_vertex_buffer(0, tooltip_effect_vertex_buffer.slice(..));

					tooltip_effect_render_pass.draw(0..6, 0..1);
					vertices
				} else {
					panic!("Tooltip existed, then didn't?")
				}
			};

			{
				let tooltip_vertex_buffer;
				let tooltip_index_buffer;
				let tooltip_text_vertex_buffer;
				let tooltip_text_index_buffer;

				let copy_bind_group;
				let copy_vertex_buffer;

				let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
					label: Some("Tooltip Render Pass"),
					color_attachments: &[Some(RenderPassColorAttachment {
						view: &view,
						resolve_target: None,
						ops: Operations {
							load: LoadOp::Load,
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

				{
					copy_bind_group = self.device.create_bind_group(&BindGroupDescriptor {
						label: Some("Copy Bind Group"),
						layout: &self.texture_reference_bind_group_layout,
						entries: &[
							BindGroupEntry {
								binding: 0,
								resource: BindingResource::TextureView(&tooltip_effect_texture_view),
							},
							BindGroupEntry {
								binding: 1,
								resource: BindingResource::Sampler(&self.sampler),
							}
						],
					});

					render_pass.set_pipeline(&self.copy_render_pipeline);
					render_pass.set_bind_group(0, &copy_bind_group, &[]);

					copy_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
						label: Some("Copy Vertex Buffer"),
						contents: unsafe { std::slice::from_raw_parts(vertices.as_ptr().cast::<u8>(), vertices.len() * 4) },
						usage: BufferUsages::VERTEX,
					});

					render_pass.set_vertex_buffer(0, copy_vertex_buffer.slice(..));
					render_pass.draw(0..6, 0..1);
				}

				{
					render_pass.set_pipeline(&self.render_pipeline);
					render_pass.set_bind_group(0, &self.diffuse_bind_group, &[]);

					tooltip_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
						label: Some("Tooltip Vertex Buffer"),
						contents: builder.vertices(),
						usage: BufferUsages::VERTEX,
					});

					tooltip_index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
						label: Some("Tooltip Index Buffer"),
						contents: builder.indices(),
						usage: BufferUsages::INDEX,
					});

					render_pass.set_vertex_buffer(0, tooltip_vertex_buffer.slice(..));
					render_pass.set_index_buffer(tooltip_index_buffer.slice(..), IndexFormat::Uint16);

					render_pass.draw_indexed(0..builder.indices_len(), 0, 0..1);
				}

				{
					render_pass.set_pipeline(&self.text_render_pipeline);
					render_pass.set_bind_group(0, &self.unicode_bind_group, &[]);

					tooltip_text_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
						label: Some("Tooltip Text Vertex Buffer"),
						contents: builder.text_vertices(),
						usage: BufferUsages::VERTEX,
					});

					tooltip_text_index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
						label: Some("Tooltip Text Index Buffer"),
						contents: builder.text_indices(),
						usage: BufferUsages::INDEX,
					});

					render_pass.set_vertex_buffer(0, tooltip_text_vertex_buffer.slice(..));
					render_pass.set_index_buffer(tooltip_text_index_buffer.slice(..), IndexFormat::Uint32);

					render_pass.draw_indexed(0..builder.text_indices_len(), 0, 0..1);
				}
			}

			self.queue.submit(Some(encoder.finish()));
		}

		let mut encoder = self.device.create_command_encoder(&CommandEncoderDescriptor { label: Some("Copy Command Encoder"), });
		{
			const COPY_VERTEX_BUFFER_DATA: &[f32] = &[
				// 0
				1.0,
				1.0,

				// 1
				-1.0,
				1.0,

				// 2
				-1.0,
				-1.0,

				// 0
				1.0,
				1.0,

				// 2
				-1.0,
				-1.0,

				// 3
				1.0,
				-1.0,
			];

			let copy_vertex_buffer;
			let copy_bind_group;

			let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
				label: Some("Copy Render Pass"),
				color_attachments: &[Some(RenderPassColorAttachment {
					view: &surface_view,
					resolve_target: None,
					ops: Operations {
						load: LoadOp::Load,
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

			copy_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
				label: Some("Copy Vertex Buffer"),
				contents: unsafe { std::slice::from_raw_parts(COPY_VERTEX_BUFFER_DATA.as_ptr().cast::<u8>(), COPY_VERTEX_BUFFER_DATA.len() * 4) },
				usage: BufferUsages::VERTEX,
			});

			copy_bind_group = self.device.create_bind_group(&BindGroupDescriptor {
				label: Some("Copy Bind Group"),
				layout: &self.texture_reference_bind_group_layout,
				entries: &[BindGroupEntry {
					binding: 0,
					resource: BindingResource::TextureView(&view),
				}, BindGroupEntry {
					binding: 1,
					resource: BindingResource::Sampler(&self.sampler),
				}],
			});

			render_pass.set_pipeline(&self.copy_render_pipeline);
			render_pass.set_bind_group(0, &copy_bind_group, &[]);

			render_pass.set_vertex_buffer(0, copy_vertex_buffer.slice(..));

			render_pass.draw(0..6, 0..1);
		}
		self.queue.submit(Some(encoder.finish()));

		surface_texture.present();

		Ok(())
	}
}
