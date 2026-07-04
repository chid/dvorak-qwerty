# Dvorak-QWERTY

> **⚠️ This project is archived and kept for historical purposes only.**
> It was built for Windows XP/Vista and is no longer actively maintained. Modern operating systems and tools now solve this problem natively — see the alternatives below.

## What This Solved

Keyboard shortcuts like Undo, Cut, Copy, and Paste are designed with the QWERTY layout in mind — they can all be performed one-handed on QWERTY, but land in awkward positions on Dvorak. This project provided Windows-only tools (circa 2005–2008) to type in Dvorak while keeping Ctrl-shortcuts in their QWERTY positions.

## Use These Instead

### Native OS Support (no install needed)

Most operating systems now include built-in Dvorak-QWERTY hybrid layouts:

- **macOS**: *System Settings → Keyboard → Input Sources* → add **Dvorak-QWERTY ⌘** — typing is Dvorak, but ⌘-shortcuts stay in QWERTY positions.
- **Linux**: `setxkbmap dvorak` for standard Dvorak; use tools like **KMonad** or **Key Mapper** for hybrid Ctrl behavior.
- **Windows**: Built-in Dvorak layout available in *Settings → Time & Language → Language → Keyboard*; use **AutoHotkey** scripts for hybrid Ctrl behavior.

### Dedicated Remapping Tools

- **[Karabiner-Elements](https://karabiner-elements.pqrs.org/)** (macOS) — Powerful key remapper; can enforce QWERTY shortcuts while typing in Dvorak.
- **[AutoHotkey](https://www.autohotkey.com/)** (Windows) — Scripting-based remapping; popular for Dvorak-QWERTY hybrids.
- **[KMonad](https://github.com/kmonad/kmonad)** (Cross-platform) — Advanced keyboard manager with layer support.

### Open-Source Projects

| Project | Platform | Approach |
|---------|----------|----------|
| [Dvorak-QWERTY-Ctrl](https://github.com/bradfeehan/Dvorak-QWERTY-Ctrl) | Windows | Custom keyboard layout that keeps QWERTY mappings when Ctrl is held |
| [Dvorak Improved (DIM)](https://github.com/neuromagus/dvorak-improved) | Windows, Linux | Modern Dvorak layout variants with ergonomic and programming focus |

### Hardware-Level (Firmware)

If you use a programmable keyboard (e.g., QMK/VIA-compatible), you can flash the Dvorak layout directly onto the firmware with a separate QWERTY layer for shortcuts — portable across any computer.

---

<details>
<summary><strong>📦 Original Project Details (Historical)</strong></summary>

## Components

### Dverty

A custom Windows keyboard layout (DLL) that behaves as standard US Dvorak but **toggles to QWERTY while the Ctrl key is held down**.

- Created by [Jeffrey Min](http://www.jeffmin.com/dverty/)
- Built with [Keyboard Layout Manager](http://www.klm32.com/)
- Supports 32-bit (`kbdverty.dll`) and 64-bit (`kbdverty64.dll`)
- Tested on Windows XP, Vista, and Vista x64

#### Installation (Dverty)

> **Note:** For 64-bit systems, use `kbdverty64.dll` and `dverty64.reg`.

1. Copy `dverty/kbdverty.dll` to `C:\WINDOWS\system32\`
2. Run `dverty/dverty.reg` to add the layout to the Windows Registry
3. Reboot Windows
4. Add the layout via Control Panel:
   - **Regional and Language Options** → **Languages** tab → **Details…** → **Add…**
   - Select **United States-Dvorak (Ctrl + Qwerty)**

### DVAssist

A Windows tray application (written in Delphi/Object Pascal) that lets you quickly switch between QWERTY and Dvorak layouts. This is a modified version of [cLabs DVAssist](http://clabs.org/dvorak.htm) adapted to switch between QWERTY and **Dverty**.

- Created by [Chris Morris (cLabs)](http://www.clabs.org/blogki/index.cgi?page=/ComputersAndTechnology/DvAssist)
- Licensed under the BSD License
- Uses a keyboard hook DLL (`DVAHOOK.DLL`) to intercept keypresses

## Project Structure

```
├── dverty/              # Dverty keyboard layout
│   ├── kbdverty.dll     # 32-bit layout DLL
│   ├── kbdverty64.dll   # 64-bit layout DLL
│   ├── dverty.reg       # 32-bit registry entries
│   ├── dverty64.reg     # 64-bit registry entries
│   └── README.txt       # Original Dverty readme
├── dvassist/            # DVAssist layout switcher
│   ├── src/             # Delphi source code
│   ├── lib/             # Dependencies (clutil, DUnit)
│   ├── bin/             # Compiled binaries
│   ├── doc/             # Documentation & license
│   ├── test/            # Unit tests (DUnit)
│   └── work/            # Development/scratch files
├── orig/                # Original Dverty website files
├── dverty.zip           # Dverty distributable archive
├── DVassist.exe         # DVAssist executable (Git LFS)
└── AUTHORS              # Original author URLs
```

## Known Issues

- Some keys in CapsLock mode may not work correctly with Dverty.

</details>

## License

- **DVAssist**: BSD License — see [dvassist/doc/license.txt](dvassist/doc/license.txt)
- **Dverty**: See [dverty/README.txt](dverty/README.txt)

## Authors

- **Jeffrey Min** — Dverty keyboard layout ([jeffmin.com](http://www.jeffmin.com/dverty/))
- **Chris Morris (cLabs)** — DVAssist ([clabs.org](http://www.clabs.org/blogki/index.cgi?page=/ComputersAndTechnology/DvAssist))
