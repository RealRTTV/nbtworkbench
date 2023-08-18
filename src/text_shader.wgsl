struct VertexInput {
    @location(0)
    position: vec2<f32>,
    @location(1)
    z_and_color: u32,
    @location(2)
    character: u32,
}

struct VertexOutput {
    @builtin(position)
    clip_position: vec4<f32>,
    @location(0)
    character: u32,
    @location(1)
    color: u32,
    @location(2)
    uv: vec2<f32>
}

@vertex
fn vs_main(input: VertexInput, @builtin(vertex_index) index: u32) -> VertexOutput {
    var output: VertexOutput;
    output.clip_position = vec4<f32>(input.position, f32(input.z_and_color & 0xFFu) / 255.0, 1.0);
    output.character = input.character;
    output.color = input.z_and_color >> 8u;
    switch (index % 4u) {
        case 0u: {
            output.uv = vec2<f32>(1.0, 0.0);
        }
        case 1u: {
            output.uv = vec2<f32>(0.0, 0.0);
        }
        case 2u: {
            output.uv = vec2<f32>(0.0,1.0);
        }
        default: {
            output.uv = vec2<f32>(1.0, 1.0);
        }
    }
    return output;
}

@group(0)
@binding(0)
var<storage> buf: array<u32>;

@fragment
fn fs_main(output: VertexOutput) -> @location(0) vec4<f32> {
    let x = u32(output.uv[0] * 16.0);
    let y = u32(output.uv[1] * 16.0);
    let offset = output.character * 32u;
    let index = offset + y * 2u + x / 8u;
    let a = (((buf[index / 4u] >> ((index % 4u) * 8u)) & 0xFFu) >> (7u - x % 8u)) & 1u;
    var alpha = 0.0;
    if (a > 0u) {
        alpha = 1.0;
    }
    return vec4<f32>(
        f32((output.color >> 16u) & 0xFFu) / 255.0,
        f32((output.color >> 8u) & 0xFFu) / 255.0,
        f32((output.color) & 0xFFu) / 255.0,
        alpha
    );
}