SECTIONS
{
  . = 0x80000000;
  .text : { *(.text) }
  .data : { *(.data) }
  .bss : { _bssstart = .; *(.bss) *(.sbss) *(COMMON); _bssend = . ;}
}
