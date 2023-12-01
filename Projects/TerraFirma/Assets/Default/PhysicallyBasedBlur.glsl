#shader vertex
#version 410 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;

out vec2 texCoordsOut;

void main()
{
    texCoordsOut = texCoords;
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 410 core

in vec2 texCoordsOut;

out float frag;

uniform sampler2D inputTexture;

void main()
{
    vec2 texelSize = 1.0 / vec2(textureSize(inputTexture, 0));
    float result = 0.0;
    for (int i = -2; i < 3; ++i) 
    {
        float x = float(i);
        for (int j = -2; j < 3; ++j) 
        {
            float y = float(j);
            vec2 offset = vec2(x, y) * texelSize;
            result += texture(inputTexture, texCoordsOut + offset).r;
        }
    }
    frag = result / 25.0f;
}
