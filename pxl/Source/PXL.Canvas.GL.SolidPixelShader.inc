const
  SolidPixelShaderCode: TShaderString =
'uniform sampler2D SourceTex;' +
'varying vec4 OutpColor;' +
'varying vec2 OutpTexCoord;' +
'void main()' +
'{' +
'  gl_FragColor = OutpColor;' +
'}';
