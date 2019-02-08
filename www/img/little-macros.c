
static int who_s_afraid_of_c_macros(
     struct inode *inode, 
     struct file *file) 
{
  int i; 

#ifdef CONFIG_INPUT_MOUSEDEV_PSAUX 
  if (imajor(inode) == MISC_MAJOR)
    i = MOUSEDEV_MIX;
  else 
#endif
  i = iminor(inode) - MOUSEDEV_MINOR_BASE;
  
  return 0; 
}
