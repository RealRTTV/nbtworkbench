# <img src="icons/nbtworkbench.png" width=48> NBT Workbench

### [Downloads & Releases here!](https://github.com/RealRTTV/nbtworkbench/releases)

NBT Workbench is an [NBT](https://wiki.vg/NBT) editing application,
the successor to [NBT Studio](https://github.com/tryashtar/nbt-studio),
which is in turn the successor to [NBTExplorer](https://github.com/jaquadro/NBTExplorer).
NBT Workbench is written completely from scratch in [Rust](https://www.rust-lang.org/) and designed to be as performant and efficient as possible.

## <img src="icons/features.png" width=16> Features
(Features marked with a star are new and not available in NBT Studio or Explorer):

* Java NBT files (`level.dat` / `hotbar.nbt`)
* Java region files (`.mca`)
  * ☆ Now supporting the new 1.21 compression format
* SNBT files (`.snbt`)
* ☆ [Web Version](https://rttv.ca/main)
* Save as dialog
* Create new nbt file / new region file
* ☆ Action wheel
  * By holding right-click over an NBT tag: A circular action wheel will appear, which will let you make specific changes to NBT tags, this includes:
  * Copying the condensed/raw or formatted/pretty SNBT version of a tag.
  * ☆ Opening an array in a preferred hex editor.
  * ☆ Opening nbt as SNBT in a preferred text editor.
  * ☆ Sorting Compounds alphabetically or by type.
* ☆ Hotkeys to quickly create new elements by their type
* ☆ Editing tag key/values in one click by simply being overtop the text.
* Tags can be selected, dragged and dropped to move them around.
* Undo and redo with Ctrl + Z and Ctrl + Y / Ctrl + Shift + Z respectively.
* ☆ Ctrl + D to duplicate the hovered tag below itself
* ☆ Searching with substrings, regex and snbt matching.
* ☆ Bookmarks
* ☆ Line Numbers
* ☆ Dark Mode
* ☆ Freehand mode to easily dive into NBT files without needing to click precisely
* Alt + Left / Right Arrow to retract and expand respectively
  * ☆ Alt + Shift + Right Arrow to expand fully
* ☆ Remastered NBT Explorer Art
* ☆ CLI Mode `nbtworkbench -?`
  * ☆ `nbtworkbench find` to search across multiple files
  * ☆ `nbtworkbench reformat` to reformat the extensions of multiple files
* Reload button
* ☆ Tabs
* ☆ The fastest NBT read / write around

# Credits
NBT Workbench was made by myself, Katie;
however, it would not come to be without the lovely projects below inspiring it.

### Design
* [NBT Studio by tryashtar](https://github.com/tryashtar/nbt-studio)
* [NBTExplorer by jaquadro](https://github.com/jaquadro/NBTExplorer)

### Technologies
* [WGPU](https://github.com/gfx-rs/wgpu)
* [Rust](https://rust-lang.org)

### Icons
* Remastered/Inspired by [jaquado](https://github.com/jaquadro)'s [NBTExplorer](https://github.com/jaquadro/NBTExplorer) icons.
