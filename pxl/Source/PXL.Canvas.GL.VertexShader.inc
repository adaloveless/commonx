const
  VertexShaderCode: TShaderString =
'attribute vec2 InPos;' +
'attribute vec2 InpTexCoord;' +
'attribute vec4 InpColor;' +
'varying vec4 OutpColor;' +
'varying vec2 OutpTexCoord;' +
'void main()' +
'{' +
'  gl_Position = vec4(InPos, 0.0, 1.0);' +
'  OutpColor = InpColor;' +
'  OutpTexCoord = InpTexCoord;' +
'}';