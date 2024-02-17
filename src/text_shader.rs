use wgsl_inline::wgsl;

wgsl! {
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
		color: vec4<f32>,
		@location(2)
		uv: vec2<f32>
	}

	@vertex
	fn vertex(input: VertexInput, @builtin(vertex_index) index: u32) -> VertexOutput {
		var output: VertexOutput;
		output.clip_position = vec4<f32>(input.position, f32(input.z_and_color & 0xFFu) / 256.0, 1.0);
		output.character = input.character;
		let raw_color = input.z_and_color >> 8u;
		var color: vec4<f32> = vec4<f32>(f32((raw_color >> 16u) & 0xFFu) / 255.0, f32((raw_color >> 8u) & 0xFFu) / 255.0, f32((raw_color) & 0xFFu) / 255.0, 1.0);
		output.color = color;
		switch (index % 4u) {
			case 0u: {
				output.uv = vec2<f32>(1.0, 0.0);
			}
			case 1u: {
				output.uv = vec2<f32>(0.0, 0.0);
			}
			case 2u: {
				output.uv = vec2<f32>(0.0, 1.0);
			}
			default: {
				output.uv = vec2<f32>(1.0, 1.0);
			}
		}
		return output;
	}

	@group(0)
	@binding(0)
	var buf: texture_2d<f32>;

	@fragment
	fn fragment(input: VertexOutput) -> @location(0) vec4<f32> {
		let x = u32(input.uv[0] * 16.0);
		let y = u32(input.uv[1] * 16.0);
		let bit_index = input.character * 256u + y * 16 + x;
		let byte = u32(textureLoad(buf, vec2<u32>(bit_index / 8u % 256u, bit_index / 8u / 256u), 0)[0] * 255.0);
		let bit = (byte >> (7u - bit_index % 8u)) & 1u;
		if (bit == 0u) {
			discard;
		}
		return input.color;
	}
}
