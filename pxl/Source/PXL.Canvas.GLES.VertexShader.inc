const
  VertexShaderCode: TShaderString =
'attribute vec4 InPos;' +
'attribute vec2 InpTexCoord;' +
'attribute vec4 InpColor;' +
'attribute vec4 ClipParams;'+
'varying vec4 OutpColor;' +
'varying vec2 OutpTexCoord;' +
'void main()' +
'{' +
'  gl_Position = vec4(InPos.x,InPos.y,InPos.z/ClipParams.w,1.0);' +
'  OutpColor = InpColor;' +
'  OutpTexCoord = InpTexCoord;' +
'}';