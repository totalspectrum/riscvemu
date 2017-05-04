SECTIONS
{
  . = 0x2000;
  .text : { *(.text) }
  .data : { *(.data) }
  .bss : { _bssstart = .; *(.bss) *(.sbss) *(COMMON); _bssend = . ;}
}
