# study-haskell

[Haskell](https://www.haskell.org) 개인공부용
- [Learn you a haskell](http://learnyouahaskell.com/chapters)

## 용어 & 개념 정리

- function application : 함수 호출.
- precedence : 함수호출이나 연산자등을 적용할때 뭐부터 할지에 대한 우선순위를 말함. 원 단어 뜻은 우선, 상위, 서열
- range : 리스트를 만들때 구성요소들의 순서나 규칙등을 정하는 방법
- type inference : 개발자가 일일이 이게 뭔 타입인지 명시하지 않아도 컴파일러가 알아서 무슨타입인지 인지함
- type declaration : 타입 선언(이름 :: 타입..)
- type variable : 타입 선언시 어떤 타입이든 관계없이 다 받을수 있다는 의미
- polymorphic function : type variable 을 가지고 있는 함수
- typeclass : 타입을 만들어내는 클래스. 상태는 없고 타입이 가져야 하는 행동만 있음. interface 비슷
- class constraint : 어떤 type variable 이 최소 어떤 typeclass 가 있어야 한다고 제약을 거는 방법.
- type annotation : 어떤 값이 무슨 타입인지 직접 명시해서 컴파일러에게 알려줌. 문법(::)이 타입 선언이랑 똑같음
- pattern matching : 어떤 데이터를 분해를 하거나, 분해해서 어떤 형태가 일치하는지 확인하는 방법. 함수의 바디 구현이나 where 구문에서 사용가능
- edge condition : 재귀가 끝나게 해주는 조건. 재귀 로직상 말이 안되는(더이상 재귀 수행이 안되는) 조건.
- partial application : 함수 호출시 모든 파라매터를 넣고 호출하는게 아니라 일부만 넣고 호출하는것.
- partially applied function : 부분적용해서 결과로 리턴된 함수
- curried function : 함수에 파라매터 1개만 적용에서 결과로 리턴된 함수
- module : typeclass 나 function 등 관련있는것들 끼리 모아서 만든 일종의 라이브러리
- prelude module : import 를 안해도 기본으로 탑재되어 있는 모듈들
- data declaration : 값을 가지고 있는 대수형 데이타 타입 선언
- value constructos : data 를 만들때 값을 받는 생성자
- record syntax : data 선언시 필드에 이름을 지정해주는 방법.
- value constructors : data 를 만들때 특정 타입의 값을 받는 생성자
- type constructors : data 를 만들때 타입을 받는 생성자
- type synonyms : 똑같은 type 이 이름이 여러개 있는것.  ex) type String = [Char]
- function composition : 함수 합성 (.) (g . f) v
- type is concrete : 어떤 타입이 확정됨을 말함.
- functor : 뭔가 감싸져 있는 상태(box)에서 매핑이 되도록 해줌. computaional context
- applicative functor : 부분 적용된 함수가 감싸져 있고 이것과 일반 functor 와 함수 적용 가능도록 해줌.
- monoid : 리스트(혹은 다른 데이터 구조)의 모든 원소를 associative binary function 적용하도록 해줌.
- monadic value : Moand 로 감싸져 있는 값(m a)
- monadic function : monadic value 에 monad 적용을 하는 함수 (\a -> m a)

## ghci
- :info  : 해당 타입이 어떤 타입클래스가 구현되있는지
- :t  : 해당 값의 타입이 뭔지
- :k  : 해당 타입(data) 또는 타입클래스(type)의 kind 가 뭔지
- :{  : 여러줄 입력 시작
- :}  : 여러줄 입력 종료

## 명령어

- compile 후 실행 파일 만들기
```sh
ghc --make filename.hs
```
- 소스를 바로 실행하기
```sh
runhaskell filename.hs
```

## 개발

### Build System

- [cabal](https://wiki.haskell.org/How_to_write_a_Haskell_program#Structure_of_a_simple_project)
- [stack](https://www.haskellstack.org/)

### Setting for Intelli J IDEA

1. `Preferences` → `Plugins` → 'haskell' 설치  
2. `File` → `new` → `Project...` → `haskell` 선택  
3. 첫실행이면 ghc, cabal, ghc-mod, ghc-modi 등 경로 설정해줌.  설치가 안되있으면   
  ```
  cabal install ghc-mod
  ```
4. `File` → `Project structure...` → `Project` -> Project SDK 를  GHC 로 변경  

### Setting for Atom

Install package :  [language-haskell](https://atom.io/packages/language-haskell), [script](https://atom.io/packages/script)


### Setting for VS Code

Install package : [Haskelly](https://marketplace.visualstudio.com/items?itemName=UCL.haskelly)

## 기타 볼만한 글
- [haskell architecture](http://www.haskellforall.com/2014/04/scalable-program-architectures.html)