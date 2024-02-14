use wgsl_inline::wgsl;

wgsl! {
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
	fn v(input: VertexInput) -> VertexOutput {
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
	fn f(input: VertexOutput) -> @location(0) vec4<f32> {
		var out = textureSample(t_diffuse, s_diffuse, input.tex_coords);
		if (out[3] == 0.0) {
		    discard;
		}

		// to srgb
		var x: f32 = out.x;
		if (x <= 0.0031308) {
			x = x * 12.92;
		} else {
			x = 1.055 * pow(x, 0.416666667) - 0.055;
		}

		var y: f32 = out.y;
		if (y <= 0.0031308) {
			y = y * 12.92;
		} else {
			y = 1.055 * pow(y, 0.416666667) - 0.055;
		}

		var z: f32 = out.z;
		if (z <= 0.0031308) {
			z = z * 12.92;
		} else {
			z = 1.055 * pow(z, 0.416666667) - 0.055;
		}

		return vec4<f32>(
			x,
			y,
			z,
			out.w,
		);
	}
}
