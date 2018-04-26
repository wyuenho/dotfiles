;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("while/else" "while ${1:expression}:\n    ${2:pass}\nelse:\n    ${3:pass}\n" "while/else" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/while-else.yasnippet" nil "d4fe927c-294c-484e-a0e2-e06d7b47bcf8")
                       ("try/finally" "try:\n    ${1:pass}\nfinally:\n    ${2:pass}\n" "try/finally" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/try-finally.yasnippet" nil "6b06c953-b903-4f94-a92a-c1ddbac77730")
                       ("try/except/finally" "try:\n    ${1:pass}\nexcept ${2:expression} as ${3:identifier}:\n    ${4:pass}\nfinally:\n    ${5:pass}\n" "try/except/finally" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/try-except-finally.yasnippet" nil "9149132f-901c-49df-8be2-f6e701bd7bb6")
                       ("try/except/else/finally" "try:\n    ${1:pass}\nexcept ${2:expression} as ${3:identifier}:\n    ${4:pass}\nelse:\n    ${5:pass}\nfinally:\n    ${6:pass}\n" "try/except/else/finally" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/try-except-else-finally.yasnippet" nil "ddb62d7a-e1c3-440c-9b84-f6512674468b")
                       ("pudb" "import pudb; pudb.set_trace()\n" "pudb" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/pudb.yasnippet" nil "115cef97-abd9-462d-9589-ae46a7713277")
                       ("pdb" "import pdb; pdb.set_trace()\n" "pdb" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/pdb.yasnippet" nil "dee698ba-e992-475d-86f6-ed65145c33f9")
                       ("ipdb" "import ipdb; ipdb.set_trace()\n" "ipdb" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/ipdb.yasnippet" nil "efaf4c48-7525-4d17-baac-962a9b04e0d9")
                       ("for/else" "for ${1:target_list} in ${2:expression_list}:\n    ${3:pass}\nelse:\n    ${4:pass}\n" "for/else" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/for-else.yasnippet" nil "e42ee1a6-abd7-4f87-b42a-ecbbc66017b5")
                       ("else" "else:\n    ${1:pass}\n" "else" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/else.yasnippet" nil "916ba8d9-138d-4493-a9d7-ef589401e7b9")
                       ("elif" "elif ${1:expression}:\n    ${2:pass}\n" "elif" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/elif.yasnippet" nil "b1fce2ba-a1b6-4fd5-8051-3a13386217dc")
                       ("def(abstract class method)" "def ${1:funcname}(self, ${2:parameter_list}):\n    raise NotImplementedError\n" "def(abstract class method)" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/def-abstract-class-method.yasnippet" nil "4b5266b5-2246-4db3-85b8-666c31bd5ead")
                       ("async/with" "async with ${1:expr} as ${2:var}:\n    ${3:block}\n" "async/with" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/async-with.yasnippet" nil "ca740e97-f002-4124-abf3-0e05e328b8d9")
                       ("async/for" "async for ${1:target} in ${2:iter}:\n    ${3:block}\n" "async/for" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/async-for.yasnippet" nil "68610417-f482-480a-80ad-308669fbec30")
                       ("async/for/else" "async for ${1:target} in ${2:iter}:\n    ${3:block}\nelse:\n    ${4:block}\n" "async/for/else" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/async-for-else.yasnippet" nil "e09ee407-8392-4e3f-8d44-972c20faa840")
                       ("async/def" "async def ${1:funcname}(${2:parameter_list}):\n    ${3:pass}\n" "async/def" nil nil nil "/Users/wyuenho/.emacs.d/snippets/python-mode/async-def.yasnippet" nil "6b2a961b-0dc4-4b7f-aaeb-a572ba5e6a2f")))


;;; Do not edit! File generated at Thu Apr 26 04:40:03 2018
