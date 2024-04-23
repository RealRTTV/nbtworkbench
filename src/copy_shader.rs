use wgsl_inline::wgsl;

wgsl! {
    struct VertexInput {
        @location(0)
        position: vec2<f32>
    }

    struct VertexOutput {
        @builtin(position)
        position: vec4<f32>,
        @location(0)
        uv: vec2<f32>,
    }

    @vertex
    fn vertex(input: VertexInput) -> VertexOutput {
        var output: VertexOutput;
        output.position = vec4<f32>(input.position, 1.0 - (254.0 / 255.0), 1.0);
        output.uv = vec2<f32>((input.position.x + 1.0) / 2.0, (1.0 - input.position.y) / 2.0);
        return output;
    }

    @group(0)
    @binding(0)
    var texture: texture_2d<f32>;

    @group(0)
    @binding(1)
    var texture_sampler: sampler;

    @fragment
    fn fragment(input: VertexOutput) -> @location(0) vec4<f32> {
        return textureSample(texture, texture_sampler, input.uv);
    }
}
