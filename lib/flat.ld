SECTIONS
{
  . = 0x4000;
  .text : { *(.text) }
  .data : { *(.data) }
  .bss : { _bssstart = .; *(.bss) *(.sbss) *(COMMON); _bssend = . ;}
}
