# study-haskell

개인공부용  
<https://www.haskell.org/documentation>  
<http://learnyouahaskell.com/chapters>

### Setting for Intelli J IDEA project

1. `Preferences` → `Plugins` → 'haskell' 설치  
2. `File` → `new` → `Project...` → `haskell` 선택  
3. 첫실행이면 ghc, cabal, ghc-mod, ghc-modi 등 경로 설정해줌.  설치가 안되있으면   
  ```sh
  cabal install ghc-mod
  ```
4. `File` → `Project structure...` → `Project` -> Project SDK 를  GHC 로 변경  

### Build System

`cabal` 을 이용함  
<https://wiki.haskell.org/How_to_write_a_Haskell_program#Structure_of_a_simple_project>  