struct VertexInput {
    @location(0)
    position: vec3<f32>,
    @location(1)
    tex_coords: vec2<f32>
}

struct VertexOutput {
    @builtin(position)
    clip_position: vec4<f32>,
    @location(0)
    tex_coords: vec2<f32>
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    output.tex_coords = input.tex_coords;
    output.clip_position = vec4<f32>(input.position, 1.0);
    return output;
}

@group(0)
@binding(0)
var t_diffuse: texture_2d<f32>;

@group(0)
@binding(1)
var s_diffuse: sampler;

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    var out = textureSample(t_diffuse, s_diffuse, input.tex_coords);
    if (out[3] == 0.0) {
       discard;
    } else {
        return out;
    }
}