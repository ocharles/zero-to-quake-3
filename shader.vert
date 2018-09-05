#version 450
#extension GL_ARB_separate_shader_objects : enable

out gl_PerVertex {
    vec4 gl_Position;
};

layout(location = 0) out vec3 fragColor;

layout(location = 0) in vec3 position;

layout(location = 1) in vec3 color;

layout(binding = 0) uniform UniformBufferObject {
    mat4 mvp;
} ubo;

void main() {
    gl_Position = ubo.mvp * vec4(position.xyz, 1);
    fragColor = color;
}
