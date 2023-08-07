use std::num::NonZeroU64;

use wgpu::*;
use wgpu::util::{BufferInitDescriptor, DeviceExt};
use winit::dpi::PhysicalSize;
use winit::event::*;
use winit::event_loop::{ControlFlow, EventLoop};
use winit::platform::windows::WindowBuilderExtWindows;
use winit::window::{Icon, Window, WindowBuilder};

use crate::{assets, NbtWorkbench};
use crate::vertex_buffer_builder::VertexBufferBuilder;

pub const WINDOW_HEIGHT: usize = 420;

pub async fn run() {
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        .with_title("NBT Workbench")
        .with_transparent(false)
        .with_inner_size(PhysicalSize::new(620, WINDOW_HEIGHT as u32))
        .with_window_icon(Some(Icon::from_rgba(assets::icon(), assets::ICON_WIDTH as u32, assets::ICON_HEIGHT as u32).expect("valid format")))
        .with_drag_and_drop(true)
        .build(&event_loop)
        .unwrap();
    let mut state = State::new(&window).await;
    let mut workbench = NbtWorkbench::new();

    event_loop.run(move |event, _, control_flow| match event {
        Event::RedrawRequested(window_id) if window_id == window.id() => {
            match state.render(&workbench) {
                Ok(()) => {},
                Err(SurfaceError::Lost) => state.surface.configure(&state.device, &state.config),
                Err(SurfaceError::OutOfMemory) => *control_flow = ControlFlow::ExitWithCode(1),
                Err(SurfaceError::Timeout) => eprintln!("Frame took too long to process"),
                Err(SurfaceError::Outdated) => {
                    eprintln!("Surface changed unexpectedly");
                    *control_flow = ControlFlow::ExitWithCode(1)
                },
            }
        },
        Event::WindowEvent { ref event, window_id, } if window_id == window.id() => {
            if !state.input(event, &mut workbench) {
                match event {
                    WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                    WindowEvent::Resized(new_size) => state.resize(&mut workbench, *new_size),
                    WindowEvent::ScaleFactorChanged { new_inner_size: new_size, .. } => state.resize(&mut workbench, **new_size),
                    _ => {}
                }
            } else {
                window.request_redraw()
            }
        }
        _ => {}
    })
}

struct State {
    surface: Surface,
    device: Device,
    queue: Queue,
    config: SurfaceConfiguration,
    render_pipeline: RenderPipeline,
    size: PhysicalSize<u32>,
    diffuse_bind_group: BindGroup,
    text_render_pipeline: RenderPipeline,
    unicode_bind_group: BindGroup,
}

impl State {
    async fn new(window: &Window) -> Self {
        let size = window.inner_size();
        let instance = Instance::new(InstanceDescriptor {
            backends: Backends::all(),
            dx12_shader_compiler: Default::default(),
        });
        let surface = unsafe { instance.create_surface(window).unwrap() };
        let adapter = instance.request_adapter(&RequestAdapterOptions {
            power_preference: PowerPreference::LowPower,
            force_fallback_adapter: false,
            compatible_surface: Some(&surface)
        }).await.unwrap();
        let (device, queue) = adapter.request_device(&DeviceDescriptor {
            features: adapter.features(),
            limits: if cfg!(target_arch = "wasm32") {
                Limits::downlevel_webgl2_defaults()
            } else {
                Limits::default()
            },
            label: None
        },
                                                     None
        ).await.unwrap();
        let config = SurfaceConfiguration {
            usage: TextureUsages::RENDER_ATTACHMENT,
            format: surface.get_capabilities(&adapter).formats[0],
            width: size.width,
            height: size.height,
            present_mode: PresentMode::AutoVsync,
            alpha_mode: CompositeAlphaMode::Auto,
            view_formats: vec![],
        };
        surface.configure(&device, &config);
        let texture_size = Extent3d {
            width: assets::ATLAS_WIDTH as u32,
            height: assets::ATLAS_HEIGHT as u32,
            depth_or_array_layers: 1,
        };
        let diffuse_texture = device.create_texture(
            &TextureDescriptor {
                size: texture_size,
                mip_level_count: 1,
                sample_count: 1,
                dimension: TextureDimension::D2,
                format: TextureFormat::Rgba8UnormSrgb,
                usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
                label: Some("Diffuse Texture"),
                view_formats: &[],
            }
        );
        queue.write_texture(
            ImageCopyTexture {
                texture: &diffuse_texture,
                mip_level: 0,
                origin: Origin3d::ZERO,
                aspect: TextureAspect::All
            },
            assets::ATLAS,
            ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(4 * assets::ATLAS_WIDTH as u32),
                rows_per_image: Some(assets::ATLAS_HEIGHT as u32),
            },
            texture_size
        );
        let diffuse_texture_view = diffuse_texture.create_view(&TextureViewDescriptor::default());
        let diffuse_sampler = device.create_sampler(&SamplerDescriptor {
            label: Some("Diffuse Sampler"),
            address_mode_u: AddressMode::Repeat,
            address_mode_v: AddressMode::Repeat,
            address_mode_w: AddressMode::Repeat,
            mag_filter: FilterMode::Linear,
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
                        sample_type: TextureSampleType::Float { filterable: true }
                    },
                    count: None
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::Filtering),
                    count: None
                }
            ],
            label: Some("Texture Bind Group Layout")
        });
        let diffuse_bind_group = device.create_bind_group(&BindGroupDescriptor {
            layout: &texture_bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: BindingResource::TextureView(&diffuse_texture_view)
                },
                BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::Sampler(&diffuse_sampler)
                }
            ],
            label: Some("Diffuse Bind Group")
        });
        let shader = device.create_shader_module(include_wgsl!("shader.wgsl"));
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
                entry_point: "vs_main",
                buffers: &[
                    VertexBufferLayout {
                        array_stride: 20,
                        step_mode: VertexStepMode::Vertex,
                        attributes: &vertex_attr_array![0 => Float32x3, 1 => Float32x2]
                    }
                ]
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(ColorTargetState {
                    format: config.format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL
                })]
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: Some(Face::Back),
                polygon_mode: PolygonMode::Fill,
                unclipped_depth: false,
                conservative: false
            },
            depth_stencil: None,
            multisample: MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false
            },
            multiview: None
        });
        let unicode_buffer = device.create_buffer_init(&BufferInitDescriptor {
            label: Some("Unicode Buffer"),
            contents: assets::UNICODE,
            usage: BufferUsages::STORAGE
        });
        let unicode_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Unicode Bind Group Layout"),
            entries: &[BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::FRAGMENT,
                ty: BindingType::Buffer {
                    ty: BufferBindingType::Storage { read_only: true },
                    has_dynamic_offset: false,
                    min_binding_size: NonZeroU64::new(assets::UNICODE.len() as u64)
                },
                count: None
            }]
        });
        let unicode_bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Unicode Bind Group"),
            layout: &unicode_bind_group_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: BindingResource::Buffer(BufferBinding {
                    buffer: &unicode_buffer,
                    offset: 0,
                    size: None
                })
            }]
        });
        let text_shader = device.create_shader_module(include_wgsl!("text_shader.wgsl"));
        let text_render_pipeline_payout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Text Render Pipeline Layout"),
            bind_group_layouts: &[&unicode_bind_group_layout],
            push_constant_ranges: &[]
        });
        let text_render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Text Render Pipeline"),
            layout: Some(&text_render_pipeline_payout),
            vertex: VertexState {
                module: &text_shader,
                entry_point: "vs_main",
                buffers: &[
                    VertexBufferLayout {
                        array_stride: 16,
                        step_mode: VertexStepMode::Vertex,
                        attributes: &vertex_attr_array![0 => Float32x3, 1 => Uint32]
                    }
                ]
            },
            fragment: Some(FragmentState {
                module: &text_shader,
                entry_point: "fs_main",
                targets: &[Some(ColorTargetState {
                    format: config.format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL
                })]
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: Some(Face::Back),
                polygon_mode: PolygonMode::Fill,
                unclipped_depth: false,
                conservative: false
            },
            depth_stencil: None,
            multisample: MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false
            },
            multiview: None
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
            unicode_bind_group
        }
    }

    fn resize(&mut self, workbench: &mut NbtWorkbench, new_size: PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.size = new_size;
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
            workbench.window_height(new_size.height as usize);
        }
    }

    fn input(&mut self, event: &WindowEvent, workbench: &mut NbtWorkbench) -> bool {
        match event {
            WindowEvent::Resized(size) => {
                workbench.window_height = size.height as usize;
                for entry in workbench.tabs.iter_mut() {
                    entry.window_height = workbench.window_height;
                    entry.scroll = entry.scroll();
                }
                false
            },
            WindowEvent::Moved(_) => false,
            WindowEvent::CloseRequested => false,
            WindowEvent::Destroyed => false,
            WindowEvent::DroppedFile(file) => workbench.on_open_file(file).is_some(),
            WindowEvent::HoveredFile(_) => false,
            WindowEvent::HoveredFileCancelled => false,
            WindowEvent::ReceivedCharacter(_) => false,
            WindowEvent::Focused(_) => false,
            WindowEvent::KeyboardInput { input, .. } => workbench.on_key_input(input),
            WindowEvent::ModifiersChanged(_) => false,
            WindowEvent::CursorMoved { position, .. } => workbench.on_cursor_move(position),
            WindowEvent::CursorEntered { .. } => false,
            WindowEvent::CursorLeft { .. } => false,
            WindowEvent::MouseWheel { delta, .. } => workbench.on_scroll(delta),
            WindowEvent::MouseInput { state, button, .. } => workbench.on_mouse_input(state, button),
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
        }
    }

    fn render(&mut self, workbench: &NbtWorkbench) -> Result<(), SurfaceError> {
        let output = self.surface.get_current_texture()?;
        let view = output.texture.create_view(&TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("Render Encoder")
        });

        {
            let vertex_buffer;
            let index_buffer;
            let text_vertex_buffer;
            let text_index_buffer;
            {
                let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                    label: Some("Render Pass"),
                    color_attachments: &[Some(RenderPassColorAttachment {
                        view: &view,
                        resolve_target: None,
                        ops: Operations {
                            load: LoadOp::Clear(Color {
                                r: 0.013,
                                g: 0.013,
                                b: 0.013,
                                a: 1.0
                            }),
                            store: true
                        }
                    })],
                    depth_stencil_attachment: None
                });

                render_pass.set_pipeline(&self.render_pipeline);
                render_pass.set_bind_group(0, &self.diffuse_bind_group, &[]);

                let mut builder = VertexBufferBuilder::new(&self.size, assets::ATLAS_WIDTH, assets::ATLAS_HEIGHT, workbench.scroll());
                workbench.render(&mut builder);

                vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
                    label: Some("Vertex Buffer"),
                    contents: builder.vertices(),
                    usage: BufferUsages::VERTEX
                });

                index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
                    label: Some("Index Buffer"),
                    contents: builder.indices(),
                    usage: BufferUsages::INDEX
                });

                render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
                render_pass.set_index_buffer(index_buffer.slice(..), IndexFormat::Uint16);

                render_pass.draw_indexed(0..builder.indices_len(), 0, 0..1);

                render_pass.set_pipeline(&self.text_render_pipeline);
                render_pass.set_bind_group(0, &self.unicode_bind_group, &[]);

                text_vertex_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
                    label: Some("Text Vertex Buffer"),
                    contents: builder.text_vertices(),
                    usage: BufferUsages::VERTEX
                });

                text_index_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
                    label: Some("Text Index Buffer"),
                    contents: builder.text_indices(),
                    usage: BufferUsages::INDEX
                });

                render_pass.set_vertex_buffer(0, text_vertex_buffer.slice(..));
                render_pass.set_index_buffer(text_index_buffer.slice(..), IndexFormat::Uint16);

                render_pass.draw_indexed(0..builder.text_indices_len(), 0, 0..1);
            }
        }

        self.queue.submit(std::iter::once(encoder.finish()));
        output.present();

        Ok(())
    }
}