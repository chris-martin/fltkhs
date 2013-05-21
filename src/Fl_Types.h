#ifndef __FL_TYPES_H
#define __FL_TYPES_H
#include "FL/fl_types.h"

#ifdef __cplusplus
EXPORT {
#endif
#ifndef INTERNAL_LINKAGE
enum Fl_Labeltype {
  FL_NORMAL_LABEL = 0,
  FL_NO_LABEL,
  _FL_SHADOW_LABEL,
  _FL_ENGRAVED_LABEL,
  _FL_EMBOSSED_LABEL,
  _FL_MULTI_LABEL,
  _FL_ICON_LABEL,
  _FL_IMAGE_LABEL,
  FL_FREE_LABELTYPE
};
#endif
typedef void* fl_Window;
typedef void* fl_Group;
typedef void* fl_Color;
typedef void* fl_Adjuster;
typedef void* fl_Bitmap;
typedef void* fl_BMP_Image;
typedef void* fl_Box;
typedef void* fl_Browser_;
typedef void* fl_Browser;
typedef void* fl_Button;
typedef void* fl_Cairo;
typedef void* fl_Cairo_Window;
typedef void* fl_Chart;
typedef void* fl_Check_Browser;
typedef void* fl_Check_Button;
typedef void* fl_Choice;
typedef void* fl_Clock;
typedef void* fl_Color_Chooser;
typedef void* fl_Counter;
typedef void* fl_Device;
typedef void* fl_Dial;
typedef void* fl_Double_Window;
typedef void* fl_draw;
typedef void* fl_Export;
typedef void* fl_File_Browser;
typedef void* fl_File_Chooser;
typedef void* fl_File_Icon;
typedef void* fl_File_Input;
typedef void* fl_Fill_Dial;
typedef void* fl_Fill_Slider;
typedef void* fl_Float_Input;
typedef void* fl_FormsBitmap;
typedef void* fl_FormsPixmap;
typedef void* fl_Free;
typedef void* fl_GIF_Image;
typedef void* fl_Gl_Window;
typedef void* fl_Help_Dialog;
typedef void* fl_Help_View;
typedef void* fl_Hold_Browser;
typedef void* fl_Hor_Fill_Slider;
typedef void* fl_Hor_Nice_Slider;
typedef void* fl_Hor_Slider;
typedef void* fl_Hor_Value_Slider;
typedef void* fl_Image;
typedef void* fl_Input_Choice;
typedef void* fl_Input_;
typedef void* fl_Input;
typedef void* fl_Int_Input;
typedef void* fl_JPEG_Image;
typedef void* fl_Light_Button;
typedef void* fl_Line_Dial;
typedef void* fl_Menu_Bar;
typedef void* fl_Menu_Button;
typedef void* fl_Menu_;
typedef void* fl_Menu;
typedef void* fl_Menu_Item;
typedef void* fl_Menu_Window;
typedef void* fl_Multi_Browser;
typedef void* fl_Multi_Label;
typedef void* fl_Multiline_Input;
typedef void* fl_Multiline_Output;
typedef void* fl_Native_File_Chooser;
typedef void* fl_Nice_Slider;
typedef void* fl_Object;
typedef void* fl_Output;
typedef void* fl_Overlay_Window;
typedef void* fl_Pack;
typedef void* fl_Paged_Device;
typedef void* fl_Pixmap;
typedef void* fl_Plugin;
typedef void* fl_PNG_Image;
typedef void* fl_PNM_Image;
typedef void* fl_Positioner;
typedef void* fl_PostScript;
typedef void* fl_Preferences;
typedef void* fl_Printer;
typedef void* fl_Progress;
typedef void* fl_Radio_Button;
typedef void* fl_Radio_Light_Button;
typedef void* fl_Radio_Round_Button;
typedef void* fl_Repeat_Button;
typedef void* fl_Return_Button;
typedef void* fl_RGB_Image;
typedef void* fl_Roller;
typedef void* fl_Round_Button;
typedef void* fl_Round_Clock;
typedef void* fl_Scrollbar;
typedef void* fl_Scroll;
typedef void* fl_Secret_Input;
typedef void* fl_Select_Browser;
typedef void* fl_Shared_Image;
typedef void* fl_show_colormap;
typedef void* fl_show_input;
typedef void* fl_Simple_Counter;
typedef void* fl_Single_Window;
typedef void* fl_Slider;
typedef void* fl_Spinner;
typedef void* fl_Sys_Menu_Bar;
typedef void* fl_Table;
typedef void* fl_Table_Row;
typedef void* fl_Tabs;
typedef void* fl_Text_Buffer;
typedef void* fl_Text_Display;
typedef void* fl_Text_Editor;
typedef void* fl_Tiled_Image;
typedef void* fl_Tile;
typedef void* fl_Timer;
typedef void* fl_Toggle_Button;
typedef void* fl_Toggle_Light_Button;
typedef void* fl_Toggle_Round_Button;
typedef void* fl_Tooltip;
typedef void* fl_Tree;
typedef void* fl_Tree_Item_Array;
typedef void* fl_Tree_Item;
typedef void* fl_Tree_Prefs;
typedef void* fl_Valuator;
typedef void* fl_Value_Input;
typedef void* fl_Value_Output;
typedef void* fl_Value_Slider;
typedef void* fl_Widget;
typedef void* fl_Window;
typedef void* fl_Wizard;
typedef void* fl_XBM_Image;
typedef void* fl_XPM_Image;
typedef void (fl_Callback )(fl_Widget, void*);
typedef fl_Callback* fl_Callback_p;
#ifdef __cplusplus
}
#endif
#endif /* #ifndef __FL_TYPES_H */