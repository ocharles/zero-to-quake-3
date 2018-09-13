#version 450
#extension GL_ARB_separate_shader_objects : enable

out gl_PerVertex {
    vec4 gl_Position;
};

layout(location = 0) out vec3 fragColor;

layout(location = 1) out vec2 uv;

layout(location = 0) in vec3 position;

layout(location = 4) in uvec4 color;

layout(binding = 0) uniform UniformBufferObject {
    mat4 mvp;
} ubo;

void main() {
    gl_Position = ubo.mvp * vec4(position.xzy, 1);
    fragColor = color.xyz * vec3(1 / 255.0);
}
