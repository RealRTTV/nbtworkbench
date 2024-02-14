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
	fn vt(input: VertexInput, @builtin(vertex_index) index: u32) -> VertexOutput {
		var output: VertexOutput;
		output.clip_position = vec4<f32>(input.position, f32(input.z_and_color & 0xFFu) / 256.0, 1.0);
		output.character = input.character;
		let raw_color = input.z_and_color >> 8u;
		var color: vec4<f32> = vec4<f32>(f32((raw_color >> 16u) & 0xFFu) / 255.0, f32((raw_color >> 8u) & 0xFFu) / 255.0, f32((raw_color) & 0xFFu) / 255.0, 1.0);
		color += 0.055;
		color /= 1.055;
		output.color = vec4<f32>(
			pow(color.x, 2.4),
			pow(color.y, 2.4),
			pow(color.z, 2.4),
			pow(color.w, 2.4),
		);
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
	var<storage> buf: array<u32>;

	@fragment
	fn ft(output: VertexOutput) -> @location(0) vec4<f32> {
		let x = u32(output.uv[0] * 16.0);
		let y = u32(output.uv[1] * 16.0);
		let offset = output.character * 32u;
		let index = offset + y * 2u + x / 8u;
		let a = (((buf[index / 4u] >> ((index % 4u) * 8u)) & 0xFFu) >> (7u - x % 8u)) & 1u;
		if (a == 0u) {
			discard;
		}
		return output.color;
	}
}
