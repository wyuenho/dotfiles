;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("thenc" ".then((${1:result}) => {\n	\n}).catch((${2:err}) => {\n	\n});\n" "thenCatch" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/thenc.yasnippet" nil "42ee09cf-3cf1-46d0-a05c-d4c87609234d")
                       ("sto" "setTimeout(() => {\n	${2}\n}, ${1:delayInms});\n" "setTimeOut" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/sto.yasnippet" nil "ab0354c8-6f34-4fbb-a3e2-440754508886")
                       ("sti" "setInterval(() => {\n	${2}\n}, ${0:intervalInms});\n" "setInterval" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/sti.yasnippet" nil "9dc63a44-827a-42ee-a7e1-fe14082d5c33")
                       ("prom" "return new Promise((resolve, reject) => {\n	${1}\n});\n" "promise" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/prom.yasnippet" nil "56a2efb9-3cef-4193-a397-b9c1ae750f86")
                       ("nfn" "const ${1:name} = (${2:params}) => {\n	${3}\n}\n" "namedFunction" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/nfn.yasnippet" nil "4977dd7d-5d40-4ce8-8d1c-117603c0a067")
                       ("imp" "import ${2:moduleName} from '${1:module}';$0\n" "import" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/imp.yasnippet" nil "088d38f6-4f70-48b2-a3e4-fa4d3434f950")
                       ("imn" "import '${1:module}';$0\n" "importNoModuleName" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/imn.yasnippet" nil "a85760c0-5807-460d-863a-4fde89ea20cc")
                       ("ime" "import * as ${2:alias} from '${1:module}';$0\n" "importEverything" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/ime.yasnippet" nil "360ba12e-4652-4fd6-9d7e-52192c13a1e8")
                       ("imd" "import { $2 } from '${1:module}';$0\n" "importDestructing" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/imd.yasnippet" nil "ef2b391f-1572-411a-a87c-232e114dce7d")
                       ("ima" "import { ${2:originalName} as ${3:alias} } from '${1:module}';$0\n" "importAs" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/ima.yasnippet" nil "e5f0d5ae-74d4-4582-ae8f-b763013fc0fd")
                       ("fre" "${1:array}.forEach(${2:currentItem} => {\n	${0}\n});\n" "forEach" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/fre.yasnippet" nil "d509606b-f88a-40bd-9075-c276e26c6b07")
                       ("fof" "for (const ${1:item} of ${2:object}) {\n	${0}\n}\n" "forOf" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/fof.yasnippet" nil "be55d32b-be30-4f27-947b-26e9a0580f72")
                       ("fin" "for (const ${1:item} in ${2:object}) {\n	${0}\n}\n" "forIn" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/fin.yasnippet" nil "6da83d87-91d8-4cc8-8f92-ac70ecf93544")
                       ("enf" "export const ${1:functionName} = (${2:params}) =>  {\n	$0\n};\n\n" "exportNamedFunction" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/enf.yasnippet" nil "c14ae5cd-de8a-4f23-b3d9-77c567dccf06")
                       ("edf" "export default (${1:params}) =>  {\n	$0\n};\n\n" "exportDefaultFunction" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/edf.yasnippet" nil "017e7394-8d20-4f51-a03d-f5c235fb231c")
                       ("ecl" "export default class ${1:className} {\n	$0\n};\n\n" "exportClass" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/ecl.yasnippet" nil "9164ede2-3bc4-4578-bd01-d3a8a5d3942c")
                       ("ece" "export default class ${1:className} extends ${2:baseclassName} {\n	$0\n};\n\n" "exportClassExtends" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/ece.yasnippet" nil "97645724-3ff8-4f29-9ef7-372b419b1cbf")
                       ("dob" "const {${2:propertyName}} = ${1:objectToDestruct};\n" "destructingObject" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/dob.yasnippet" nil "4dbf8147-dd77-47b2-8de4-5d085488abe8")
                       ("dar" "const [${2:propertyName}] = ${1:arrayToDestruct};\n" "destructingArray" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/dar.yasnippet" nil "b55453dc-e741-43f2-9e1f-10456209990b")
                       ("cwa" "console.warn(${1:object});\n" "consoleWarn" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cwa.yasnippet" nil "44d740ba-7365-429e-955e-932ff48b3654")
                       ("ctr" "console.trace(${1:object});\n" "consoleTrace" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/ctr.yasnippet" nil "a94f6426-df60-4101-9d0c-63e90333dd09")
                       ("clt" "console.table(${1:object});\n" "consoleTable" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/clt.yasnippet" nil "7dd20ba2-9e39-4413-a6a6-e02e28065352")
                       ("clo" "console.log('${1:object} :', ${1:object});\n" "consoleLogObject" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/clo.yasnippet" nil "445e3a21-4be5-4d86-91ba-86f53beafb0a")
                       ("cin" "console.info(${1:object});\n" "consoleInfo" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cin.yasnippet" nil "a4572296-e3b5-4721-80c1-bcf87615b4db")
                       ("cgr" "console.group(\"${1:label}\");\n" "consoleGroup" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cgr.yasnippet" nil "1a72e55d-8c26-4fbf-9656-f1d47fdf58a2")
                       ("cge" "console.groupEnd();\n" "consoleGroupEnd" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cge.yasnippet" nil "8e653877-0bf1-470b-8aad-1845de355537")
                       ("cer" "console.error(${1:object});\n" "consoleError" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cer.yasnippet" nil "fe930588-3180-4b2a-90ec-7468a5be8b95")
                       ("cdi" "console.dir(${1:object});\n" "consoleDir" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cdi.yasnippet" nil "286dbda1-9954-4a75-b0e6-9c232348f4c2")
                       ("cco" "console.count(${1:label});\n" "consoleCount" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cco.yasnippet" nil "b82b397d-5944-4353-9a7d-4dc6ceb9f509")
                       ("ccl" "console.clear();\n" "consoleClear" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/ccl.yasnippet" nil "87d74ee1-b8c3-4f39-9525-0b1214dbd91b")
                       ("cas" "console.assert(${1:expression}, ${2:object});\n" "consoleAssert" nil nil nil "/Users/wyuenho/.emacs.d/snippets/js-mode/cas.yasnippet" nil "2363179e-f2e1-4de5-ad53-13e6af1d3644")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("ttems" "expect(() => {\n	$0\n}).toThrowErrorMatchingSnapshot();\n" "toThrowErrorMatchingSnapshot" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/ttems.yasnippet" nil "4dd1c3ea-9d02-4819-b525-8f0990bc9da0")
                       ("tte" "expect(() => {\n	$0\n}).toThrowError($1);\n" "toThrowError" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tte.yasnippet" nil "853f514c-5271-4267-a0ca-2763ddc2ae0a")
                       ("tt" "expect(() => {\n	$0\n}).toThrow();\n" "toThrow" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tt.yasnippet" nil "e11bd6c9-7019-41ef-ae72-d410f1e6a4fb")
                       ("tms" "expect($1).toMatchSnapshot();$0\n" "toMatchSnapshot" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tms.yasnippet" nil "fad07900-3dd1-42e0-badb-2f678920cf15")
                       ("tmo" "expect($1).toMatchObject($2);$0\n" "toMatchObject" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tmo.yasnippet" nil "bfcd7a06-3085-452e-a6c1-2486285c8372")
                       ("tm" "expect($1).toMatch($2);$0\n" "toMatch" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tm.yasnippet" nil "70137e04-47a3-4150-9500-ecb74f5cee87")
                       ("thp" "expect($1).toHaveProperty(${2:keyPath}, ${3:value});$0\n" "toHaveProperty" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/thp.yasnippet" nil "449c7013-970e-4c11-9afb-6d51c4d012c9")
                       ("thl" "expect($1).toHaveLength($2);$0\n" "toHaveLength" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/thl.yasnippet" nil "2d0584c5-3927-4751-a1dc-40b508e3c5bc")
                       ("thblcw" "expect($1).toHaveBeenLastCalledWith($2);$0\n" "toHaveBeenLastCalledWith" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/thblcw.yasnippet" nil "a4bdac00-3de0-414b-ac0b-f959fb6ad41f")
                       ("thbcw" "expect($1).toHaveBeenCalledWith($2);$0\n" "toHaveBeenCalledWith" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/thbcw.yasnippet" nil "654867c1-d275-4013-9c4e-ffe359195191")
                       ("thbct" "expect($1).toHaveBeenCalledTimes($2);$0\n" "toHaveBeenCalledTimes" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/thbct.yasnippet" nil "b60ef30b-2ecd-4ff0-944e-90a13ae39a06")
                       ("thbc" "expect($1).toHaveBeenCalled();$0\n" "toHaveBeenCalled" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/thbc.yasnippet" nil "5e055c8b-633c-4223-a551-89ebb2765659")
                       ("tests" "test.skip('should ${1:behave...}', () => {\n	$0\n});\n" "test.skip" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tests.yasnippet" nil "0d7acec7-aecb-406c-a493-4932700832df")
                       ("testo" "test.only('should ${1:behave...}', () => {\n	$0\n});\n" "test.only" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/testo.yasnippet" nil "795600f8-f040-4f0a-81a6-5800a29cd071")
                       ("test" "test('should ${1:behave...}', () => {\n	$0\n});\n" "test" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/test.yasnippet" nil "f404afcc-db8f-4d8d-bb55-d1eb03007136")
                       ("te" "expect($1).toEqual($2);$0\n" "toEqual" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/te.yasnippet" nil "37456e32-9160-4a5a-a802-ad937459f73b")
                       ("tce" "expect(${1:list}).toContainEqual(${2:item});$0\n" "toContainEqual" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tce.yasnippet" nil "f50941e4-bc0e-430d-b6da-283e9ad98e65")
                       ("tc" "expect(${1:list}).toContain(${2:item});$0\n" "toContain" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tc.yasnippet" nil "44785f2a-76f2-49ae-920c-00cee7e6b05c")
                       ("tbu" "expect($1).toBeUndefined();$0\n" "toBeUndefined" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbu.yasnippet" nil "719600d8-83f8-47db-9e30-ab8704d0bccc")
                       ("tbt" "expect($1).toBeTruthy();$0\n" "toBeTruthy" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbt.yasnippet" nil "389e95bd-1a53-4d66-a06f-b34593b502a5")
                       ("tbn" "expect($1).toBeNull();$0\n" "toBeNull" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbn.yasnippet" nil "3ba81cc2-63f7-4e9c-b253-ed1fa5db2efc")
                       ("tblte" "expect($1).toBeLessThanOrEqual($2);$0\n" "toBeLessThanOrEqual" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tblte.yasnippet" nil "c8cfb7fc-2186-40f2-8d69-27b4f74f0acb")
                       ("tblt" "expect($1).toBeLessThan($2);$0\n" "toBeLessThan" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tblt.yasnippet" nil "1b47c3c3-04d1-429a-a5e1-8c2c763eb26c")
                       ("tbi" "expect($1).toBeInstanceOf($2);$0\n" "toBeInstanceOf" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbi.yasnippet" nil "d4bbcfe9-6fd1-471a-aa26-102bd86a800e")
                       ("tbgte" "expect($1).toBeGreaterThanOrEqual($2);$0\n" "toBeGreaterThanOrEqual" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbgte.yasnippet" nil "8c8bfa51-2a84-4333-9ebc-c7938b3f07cc")
                       ("tbgt" "expect($1).toBeGreaterThan($2);$0\n" "toBeGreaterThan" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbgt.yasnippet" nil "b3ce92ad-3d29-4994-96b3-77e164c7ab44")
                       ("tbf" "expect($1).toBeFalsy();$0\n" "toBeFalsy" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbf.yasnippet" nil "4275266e-6652-421c-818d-7a001a502e3d")
                       ("tbd" "expect($1).toBeDefined();$0\n" "toBeDefined" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbd.yasnippet" nil "76040096-c20c-4336-9d31-ca869968459d")
                       ("tbct" "expect($1).toBeCloseTo(${2:number}, ${3:delta});$0\n" "toBeCloseTo" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tbct.yasnippet" nil "0cd653bc-aaba-4f5c-95ed-0ed5c9878df9")
                       ("tb" "expect($1).toBe($2);$0\n" "toBe" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/tb.yasnippet" nil "01f7feae-99e5-4dac-b896-b655ceb6dc12")
                       ("its" "it.skip('should ${1:behave...}', () => {\n	$0\n});\n" "it.skip" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/its.yasnippet" nil "eac838e6-eef8-4122-aee2-4661e852b249")
                       ("ito" "it.only('should ${1:behave...}', () => {\n	$0\n});\n" "it.only" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/ito.yasnippet" nil "9ca5f4d0-a10c-4a25-98b0-01b2c67b754c")
                       ("it" "it('should ${1:behave...}', () => {\n	$0\n});\n" "it" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/it.yasnippet" nil "5d115b88-3f08-41ce-9152-f8f2957fdcfe")
                       ("exprj" "expect($1).rejects$0\n" "expect.rejects" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/exprj.yasnippet" nil "b69846b6-d69c-4ea3-a932-1c414170f7ea")
                       ("expr" "expect($1).resolves$0\n" "expect.resolves" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/expr.yasnippet" nil "35310fe8-a0e1-4637-8198-ec2bffce3cd8")
                       ("exp" "expect($1)$0\n" "expect" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/exp.yasnippet" nil "7f45f7f9-9166-465d-98c1-b6c069b00b38")
                       ("descs" "describe.skip('${1:Name of the group}', () => {\n	$0\n});\n" "describe.skip" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/descs.yasnippet" nil "3583190f-c120-4f1b-9802-fd73c5b226d9")
                       ("desco" "describe.only('${1:Name of the group}', () => {\n	$0\n});\n" "describe.only" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/desco.yasnippet" nil "d7dd6ea7-15d5-431f-a609-6fd8c5c8720a")
                       ("desc" "describe('${1:Name of the group}', () => {\n	$0\n});\n" "describe" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/desc.yasnippet" nil "62d4d600-683d-4bf3-b638-b2c104c13a0b")
                       ("cut" "describe('${1:Name of the group}', () => {\n\n	let ${2:cut};\n\n	beforeEach(() => {\n		this.$2 = $3;\n	});\n\n	test('should ${4:behave...}', () => {\n		expect(this.$2).toBe($0);\n	});\n\n});\n" "template:cut" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/cut.yasnippet" nil "000d0d06-8a78-4122-b406-1435f36a61df")
                       ("be" "beforeEach(() => {\n	$0\n});\n" "beforeEach" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/be.yasnippet" nil "4df001f7-bb13-421e-b61f-5df6833373aa")
                       ("ba" "beforeAll(() => {\n	$0\n});\n" "beforeAll" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/ba.yasnippet" nil "29ef6fb2-1fd1-4284-944b-3325a39e128c")
                       ("ae" "afterEach(() => {\n	$0\n});\n" "afterEach" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/ae.yasnippet" nil "ff034595-db2b-4201-8b3b-dceb294570ad")
                       ("aa" "afterAll(() => {\n	$0\n});\n" "afterAll" nil
                        ("Jest")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/Jest/aa.yasnippet" nil "55985f97-ddda-4be6-9fcf-4c18f8b0b191")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("slr" "const $1 = () => {\n	return (\n		$2\n	);\n}\n\nexport default $1;\n" "Stateless Component Return" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/slr.yasnippet" nil "6ab22928-4c5a-46fe-9812-e192469c0ed3")
                       ("slc" "function $1($2) {\n	$3\n}\n\nexport default $1;\n" "Stateless Component Function" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/slc.yasnippet" nil "e83a7224-9463-4457-8d91-4a111359b3c0")
                       ("sl" "const $1 = () => (\n	$2\n);\n\nexport default $1;\n" "Stateless Component" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/sl.yasnippet" nil "fac694b2-f37e-4cb5-8011-1096f3c9e415")
                       ("scu" "shouldComponentUpdate(nextProps, nextState, nextContext) {\n	 $1\n}\n" "ShouldComponentUpdate" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/scu.yasnippet" nil "3e857c05-33f3-4914-8027-09e093b5177d")
                       ("rrd" "export default (state = $1, action) => {\n	switch (action.type) {\n		case $2:\n			$3\n		default:\n			return state;\n	}\n};\n" "Redux Reducer" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/rrd.yasnippet" nil "d1005984-0762-4f69-8029-6fae4daf1568")
                       ("rpf" "export const $1 = '$1';\n\nexport function $2($3) {\n	return {\n		type: $1,\n		$3\n	}\n}\n" "Redux Pure Function" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/rpf.yasnippet" nil "b5d3bc6e-787c-4777-b765-5ae0c81e7690")
                       ("rpc" "export const $1 = '$1';\n\nexport const $2 = $3 => ({\n	type: $1,\n	$3\n});\n" "Redux Pure Function Const" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/rpc.yasnippet" nil "7999730b-7171-437a-aef4-04d915804050")
                       ("rct" "export const $1 = '$1';\n" "Redux constant" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/rct.yasnippet" nil "20f19e94-56bd-4bd2-817f-7aae48a70a5c")
                       ("pcsf" "type P = {\n	$1\n};\n\ntype S = {\n	$2\n};\n\nclass $3 extends PureComponent<P, S> {\n	state = { $4 }\n	render() {\n		return (\n			$5\n		);\n	}\n}\n\nexport default $3;\n" "PureComponent Class FlowType" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/pcsf.yasnippet" nil "ea50f6c8-2d2e-4902-b1e5-b657aebef8f2")
                       ("pcs" "class $1 extends PureComponent {\n	state = { $2 }\n	render() {\n		return (\n			$3\n		);\n	}\n}\n\nexport default $1;\n" "PureComponent Class" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/pcs.yasnippet" nil "1cc27a5b-a534-4d47-aed5-83ef9489935e")
                       ("pccs" "class $1 extends PureComponent {\n	constructor(props) {\n		super(props);\n		this.state = { $2 };\n	}\n	render() {\n		return (\n			$3\n		);\n	}\n}\n\nexport default $1;\n" "PureComponent Class With Constructor" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/pccs.yasnippet" nil "154209f0-f6a5-4f6a-bc94-60fcb3ca94e3")
                       ("ipt" "import PropTypes from 'prop-types';\n" "Import PropTypes" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/ipt.yasnippet" nil "30befe6d-102e-4f53-bf4d-6ad526886d6c")
                       ("imrpc" "import React, { PureComponent } from 'react';\n" "Import react pure component" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/imrpc.yasnippet" nil "83a54a95-c5eb-4b57-8693-dfbb76bc3182")
                       ("imrc" "import React, { Component } from 'react';\n" "Import react component" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/imrc.yasnippet" nil "c48e9875-1f17-43e9-aac2-a62926fa578d")
                       ("imr" "import React from 'react';\n" "Import react" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/imr.yasnippet" nil "d64d2703-beff-4981-bceb-9ee29c46da4c")
                       ("gds" "static getDerivedStateFromProps(nextProps, prevState) {\n	$1\n}\n" "getDerivedStateFromProps" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/gds.yasnippet" nil "f909e872-3e27-4109-8aff-9d6fe890266c")
                       ("edstyc" "export default styled.$1`\n	$2\n`\n" "Export default Styled Component" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/edstyc.yasnippet" nil "2707e986-ec14-44b3-80f2-1d9caedb3019")
                       ("edl" "// eslint-disable-line\n" "EslintDisableLine" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/edl.yasnippet" nil "721a3472-6dd3-4e52-9f55-b053a5a7dcaa")
                       ("edccs" "export default class $1 extends Component {\n	state = { $2 }\n	render() {\n		return (\n			$3\n		);\n	}\n}\n" "Export default Component Class" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/edccs.yasnippet" nil "d4abb7b7-636e-49b1-b6be-d172cec16de7")
                       ("ed" "export default $1;\n" "Export default" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/ed.yasnippet" nil "905460a7-d5d2-4ad3-a726-c9011675489a")
                       ("cwum" "componentWillUnmount() {\n	$1\n}\n" "ComponentWillUnmount" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cwum.yasnippet" nil "2ffc2f44-f931-4e70-9bf5-4004d482b871")
                       ("cwu" "componentWillUpdate() {\n	$1\n}\n" "ComponentWillUpdate" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cwu.yasnippet" nil "f914a574-5ccf-4f20-b11f-54512756a820")
                       ("cwrp" "componentWillReceiveProps(nextProps) {\n	$1\n}\n" "ComponentWillReceiveProps" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cwrp.yasnippet" nil "b3648b5b-7862-4052-b976-7f644caaac56")
                       ("cwm" "componentWillMount() {\n	$1\n}\n" "ComponentWillMount" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cwm.yasnippet" nil "824b51d2-e7f4-43df-9ca2-9dd6781b5e0e")
                       ("crr" "import { connect } from 'react-redux';\n" "Connect Redux" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/crr.yasnippet" nil "7eee3386-4556-4bb3-a93d-f282f6d6eee5")
                       ("cdu" "componentDidUpdate(prevProps, prevState) {\n	$1\n}\n" "ComponentDidUpdate" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cdu.yasnippet" nil "06230a36-9f46-4581-8d31-8809c2396d18")
                       ("cdm" "componentDidMount() {\n	$1\n}\n" "ComponentDidMount" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cdm.yasnippet" nil "d7e819a4-23ee-47a3-8d70-71537fb23456")
                       ("cdc" "componentDidCatch(error, info) {\n	$1\n}\n" "ComponentDidCatch" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cdc.yasnippet" nil "8dc194a8-3504-4fe3-beda-41fb43eae53d")
                       ("cct" "const $1Context = createContext($2);\n\nclass $1Provider extends Component {\n	state = {\n		$3\n	}\n\n	render() {\n		return (\n			<$1Context.Provider value={{ state: { $3 }, actions: {} }}>\n				{this.props.children}\n			</$1Context.Provider>\n		);\n	}\n}\n\nexport default $1Provider;\n" "Create Context" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cct.yasnippet" nil "aa59b4ef-8233-4ccf-b7e2-40bf2211ea09")
                       ("ccsr" "class $1 extends Component {\n	state = { $2 }\n	render() {\n		return (\n			$3\n		);\n	}\n}\n\nexport default connect($4, $5)($1);\n" "Component Class With Redux" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/ccsr.yasnippet" nil "13cc849f-0320-4fa1-b479-c558f45aace0")
                       ("ccsf" "type P = {\n	$1\n};\n\ntype S = {\n	$2\n};\n\nclass $3 extends Component<P, S> {\n	state = { $4 }\n	render() {\n		return (\n			$5\n		);\n	}\n}\n\nexport default $3;\n" "Component Class FlowType" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/ccsf.yasnippet" nil "b0cec5bd-25b1-4c0f-9b2f-ef36f1eaefb6")
                       ("ccs" "class $1 extends Component {\n	state = { $2 }\n	render() {\n		return (\n			$3\n		);\n	}\n}\n\nexport default $1;\n" "Component Class" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/ccs.yasnippet" nil "5966ace0-5124-4364-845d-8fe98736a8b0")
                       ("cccs" "class $1 extends Component {\n	constructor(props) {\n		super(props);\n		this.state = { $2 };\n	}\n	render() {\n		return (\n			$3\n		);\n	}\n}\n\nexport default $1;\n" "Component Class With Constructor" nil
                        ("React")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/React/cccs.yasnippet" nil "19fa0363-088e-4c81-937e-9ce861187ef0")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("expect" "expect(${1:value}).to.;$0\n" "expect" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.yasnippet" nil "4d168357-7c92-4176-bd03-2a610b27f1a2")
                       ("expect.to.throw" "expect(${1:value}).to.throw(${2:error});$0\n" "expect.to.throw" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.throw.yasnippet" nil "513c9453-b6ed-45d7-9d7f-fb59bfdb5199")
                       ("expect.to.satisfy" "expect(${1:value}).to.satisfy(${2:function});$0\n" "expect.to.satisfy" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.satisfy.yasnippet" nil "a7f4e6ed-0370-433a-959c-9bfa0201843f")
                       ("expect.to.respondTo" "expect(${1:value}).to.respondTo(${2:string});$0\n" "expect.to.respondTo" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.respondTo.yasnippet" nil "379270c3-b94a-4654-9a83-12f7718eb718")
                       ("expect.to.not.throw" "expect(${1:value}).to.not.throw(${2:error});$0\n" "expect.to.not.throw" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.throw.yasnippet" nil "884531f2-c200-46c6-9723-f40d1ff54dd5")
                       ("expect.to.not.exist" "expect(${1:value}).to.not.exist;$0\n" "expect.to.not.exist" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.exist.yasnippet" nil "266aa3d6-4055-4d91-81a9-9db284a7a8a1")
                       ("expect.to.not.equal" "expect(${1:value}).to.not.equal(${2:value});$0\n" "expect.to.not.equal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.equal.yasnippet" nil "8df39974-0d43-47aa-9fb2-334310cdeee2")
                       ("expect.to.not.eql" "expect(${1:value}).to.not.eql(${2:value});$0\n" "expect.to.not.eql" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.eql.yasnippet" nil "3d4eb838-f66a-48d1-8aec-0b41f63a8453")
                       ("expect.to.not.be.undefined" "expect(${1:value}).to.not.be.undefined;$0\n" "expect.to.not.be.undefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.be.undefined.yasnippet" nil "0513e33d-2192-417e-ab71-64f475530208")
                       ("expect.to.not.be.true" "expect(${1:value}).to.not.be.true;$0\n" "expect.to.not.be.true" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.be.true.yasnippet" nil "675e3f40-28b3-44c3-b605-5eba2252190e")
                       ("expect.to.not.be.ok" "expect(${1:value}).to.not.be.ok;$0\n" "expect.to.not.be.ok" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.be.ok.yasnippet" nil "d40bb79e-1033-4564-b4d5-93f67ef0a35e")
                       ("expect.to.not.be.null" "expect(${1:value}).to.not.be.null;$0\n" "expect.to.not.be.null" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.be.null.yasnippet" nil "18862779-0d25-45a0-baa6-1b92e614df2a")
                       ("expect.to.not.be.false" "expect(${1:value}).to.not.be.false;$0\n" "expect.to.not.be.false" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.be.false.yasnippet" nil "eebb6d41-a41b-4013-89d5-f2b7e1e0bdf6")
                       ("expect.to.not.be.empty" "expect(${1:value}).to.not.be.empty;$0\n" "expect.to.not.be.empty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.not.be.empty.yasnippet" nil "bb78d98c-294b-4071-92a7-b6ada2da7e4a")
                       ("expect.to.match" "expect(${1:value}).to.match(/${2:regex}/);$0\n" "expect.to.match" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.match.yasnippet" nil "afed585d-0a7a-4b05-be28-1aca4f094566")
                       ("expect.to.include" "expect(${1:value}).to.include(${2:value});$0\n" "expect.to.include" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.include.yasnippet" nil "3775b54d-8a6d-410b-bbde-1aae01ee6506")
                       ("expect.to.include.keys" "expect(${1:value}).to.include.keys(\"${2:string}\");$0\n" "expect.to.include.keys" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.include.keys.yasnippet" nil "9f0e4c28-c5d1-43d0-a27c-c798b5f85491")
                       ("expect.to.have.string" "expect(${1:value}).to.have.string(\"${2:string}\");$0\n" "expect.to.have.string" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.string.yasnippet" nil "eb49e68a-0da2-4784-a785-5f9bd45076a3")
                       ("expect.to.have.property" "expect(${1:value}).to.have.property(\"${2:value}\");$0\n" "expect.to.have.property" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.property.yasnippet" nil "19213987-7e85-4669-a592-f35a46c86811")
                       ("expect.to.have.ownProperty" "expect(${1:value}).to.have.ownProperty(\"${2:value}\");$0\n" "expect.to.have.ownProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.ownProperty.yasnippet" nil "bcab9730-1d09-4271-bea6-0530bdc0785b")
                       ("expect.to.have.length" "expect(${1:value}).to.have.length(${2:value});$0\n" "expect.to.have.length" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.length.yasnippet" nil "763c1295-37fe-44c1-bb7e-f3ef699cd938")
                       ("expect.to.have.length.within" "expect(${1:value}).to.have.length.within(${2:start}, ${3:finish});$0\n" "expect.to.have.length.within" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.length.within.yasnippet" nil "22fdaad5-db7d-42ae-a0f9-aa91792c7734")
                       ("expect.to.have.length.of.at.most" "expect(${1:value}).to.have.length.of.at.most(${2:value});$0\n" "expect.to.have.length.of.at.most" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.length.of.at.most.yasnippet" nil "89de63c9-7679-43d3-8c2b-faba3b2ed49b")
                       ("expect.to.have.length.below" "expect(${1:value}).to.have.length.below(${2:value});$0\n" "expect.to.have.length.below" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.length.below.yasnippet" nil "52642acd-bfe4-4ba6-ae3f-c08c1535290a")
                       ("expect.to.have.length.above" "expect(${1:value}).to.have.length.above(${2:value});$0\n" "expect.to.have.length.above" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.length.above.yasnippet" nil "2daeec2f-ccea-45ff-88a5-220c7b6ab88e")
                       ("expect.to.have.keys" "expect(${1:value}).to.have.keys(\"${2:string}\");$0\n" "expect.to.have.keys" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.keys.yasnippet" nil "ca61b9bf-5b0b-4747-ba00-a9081746552d")
                       ("expect.to.have.deep.property" "expect(${1:value}).to.have.deep.property(\"${2:value}\");$0\n" "expect.to.have.deep.property" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.have.deep.property.yasnippet" nil "86684cdd-0f36-446a-970e-896c8aa0a5a3")
                       ("expect.to.exist" "expect(${1:value}).to.exist;$0\n" "expect.to.exist" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.exist.yasnippet" nil "2ec7c9cb-09e8-4552-8882-8290f7ac1efa")
                       ("expect.to.equal" "expect(${1:value}).to.equal(${2:value});$0\n" "expect.to.equal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.equal.yasnippet" nil "6dd12eea-8a43-4c72-b94c-8f88cb1e751e")
                       ("expect.to.eql" "expect(${1:value}).to.eql(${2:value});$0\n" "expect.to.eql" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.eql.yasnippet" nil "129f3433-80b7-4743-afd3-b1c16f34efa8")
                       ("expect.to.contain" "expect(${1:value}).to.contain(\"${2:string}\");$0\n" "expect.to.contain" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.contain.yasnippet" nil "428d1565-e6ac-48dc-b1ea-f40c0056bd72")
                       ("expect.to.be.within" "expect(${1:value}).to.be.within(${2:start}, ${3:finish});$0\n" "expect.to.be.within" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.within.yasnippet" nil "ac86b550-832b-4b43-842a-de039d035378")
                       ("expect.to.be.undefined" "expect(${1:value}).to.be.undefined;$0\n" "expect.to.be.undefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.undefined.yasnippet" nil "9a3c20c3-6a79-4c41-97ea-f3523ec176e3")
                       ("expect.to.be.true" "expect(${1:value}).to.be.true;$0\n" "expect.to.be.true" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.true.yasnippet" nil "d5ba999a-97f4-4bfe-bd91-3b7b85881982")
                       ("expect.to.be.ok" "expect(${1:value}).to.be.ok;$0\n" "expect.to.be.ok" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.ok.yasnippet" nil "e5a4fa52-daab-49d9-b900-8e509983812e")
                       ("expect.to.be.null" "expect(${1:value}).to.be.null;$0\n" "expect.to.be.null" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.null.yasnippet" nil "57a1675a-d8ea-4a5d-a0c7-f9fb0fe9caa7")
                       ("expect.to.be.false" "expect(${1:value}).to.be.false;$0\n" "expect.to.be.false" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.false.yasnippet" nil "31d507cf-8cbf-4f17-a104-4433ea8cd366")
                       ("expect.to.be.empty" "expect(${1:value}).to.be.empty;$0\n" "expect.to.be.empty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.empty.yasnippet" nil "db925c9a-8e3a-4f4b-82a3-8b0a7ce45ea9")
                       ("expect.to.be.closeTo" "expect(${1:value}).to.be.closeTo(${2:expected}, ${3:delta});$0\n" "expect.to.be.closeTo" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.closeTo.yasnippet" nil "501a1e2e-c2ff-4800-94b5-413012866554")
                       ("expect.to.be.below" "expect(${1:value}).to.be.below(${2:value});$0\n" "expect.to.be.below" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.below.yasnippet" nil "5704507b-c290-4ac4-af2d-44a3f712bbed")
                       ("expect.to.be.at.most" "expect(${1:value}).to.be.at.most(${2:value});$0\n" "expect.to.be.at.most" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.at.most.yasnippet" nil "1c5e5a74-8449-48f5-8734-01cc9cc4a32f")
                       ("expect.to.be.at.least" "expect(${1:value}).to.be.at.least(${2:value});$0\n" "expect.to.be.at.least" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.at.least.yasnippet" nil "aba17ca6-180b-47ce-ac5a-a876538bad53")
                       ("expect.to.be.arguments" "expect(${1:value}).to.be.arguments(${2:value});$0\n" "expect.to.be.arguments" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.arguments.yasnippet" nil "3b41b1b2-6ba3-49a4-9b35-6c82f3bd11ce")
                       ("expect.to.be.an" "expect(${1:value}).to.be.an(\"${2:type}\");$0\n" "expect.to.be.an" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.an.yasnippet" nil "3c0f1788-3d23-409c-97a3-8d55ef5428fc")
                       ("expect.to.be.an.instanceof" "expect(${1:value}).to.be.an.instanceof(${2:object});$0\n" "expect.to.be.an.instanceof" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.an.instanceof.yasnippet" nil "ec3622dd-9c13-450a-91af-d5bacfa65d54")
                       ("expect.to.be.above" "expect(${1:value}).to.be.above(${2:value});$0\n" "expect.to.be.above" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.above.yasnippet" nil "30ed0090-4829-46f6-b384-37e800b1348d")
                       ("expect.to.be.a" "expect(${1:value}).to.be.a(\"${2:type}\");$0\n" "expect.to.be.a" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect.to.be.a.yasnippet" nil "18bb02af-d665-4ac5-b0a1-fe2399404317")
                       ("expect.to.throw" "expect(${1:value}).to.throw(${2:error});$0\n" "expect.to.throw" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-throw.yasnippet" nil "be236853-dffa-45a0-957e-6eb00a43b82b")
                       ("expect.to.satisfy" "expect(${1:value}).to.satisfy(${2:function});$0\n" "expect.to.satisfy" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-satisfy.yasnippet" nil "44ef6347-a619-4067-94f5-efaf0b158746")
                       ("expect.to.respondTo" "expect(${1:value}).to.respondTo(${2:string});$0\n" "expect.to.respondTo" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-respond-to.yasnippet" nil "cd2bb24a-c182-49a2-97bd-be9cccc42e80")
                       ("expect.to.not.throw" "expect(${1:value}).to.not.throw(${2:error});$0\n" "expect.to.not.throw" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-throw.yasnippet" nil "f9d12ce6-f05e-4c77-af8f-070984a77113")
                       ("expect.to.not.exist" "expect(${1:value}).to.not.exist;$0\n" "expect.to.not.exist" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-exist.yasnippet" nil "fa77739b-fd6d-4ea2-ba1c-cdece4f18e5a")
                       ("expect.to.not.equal" "expect(${1:value}).to.not.equal(${2:value});$0\n" "expect.to.not.equal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-equal.yasnippet" nil "59df98af-416c-43a4-adf4-6444f7a235b2")
                       ("expect.to.not.eql" "expect(${1:value}).to.not.eql(${2:value});$0\n" "expect.to.not.eql" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-eql.yasnippet" nil "b01e37f7-a380-465d-b9df-16c9b94d48c9")
                       ("expect.to.not.be.undefined" "expect(${1:value}).to.not.be.undefined;$0\n" "expect.to.not.be.undefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-be-undefined.yasnippet" nil "919a1f6e-2e85-4c3e-a7e5-201febe95803")
                       ("expect.to.not.be.true" "expect(${1:value}).to.not.be.true;$0\n" "expect.to.not.be.true" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-be-true.yasnippet" nil "19a0bc04-4b3b-422e-b4b5-39d05bb5ccc4")
                       ("expect.to.not.be.ok" "expect(${1:value}).to.not.be.ok;$0\n" "expect.to.not.be.ok" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-be-ok.yasnippet" nil "0dfc20ec-27a0-419e-9bb9-b5caeda67c72")
                       ("expect.to.not.be.null" "expect(${1:value}).to.not.be.null;$0\n" "expect.to.not.be.null" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-be-null.yasnippet" nil "0fe33e12-dabc-4747-ae66-27ba09b2795e")
                       ("expect.to.not.be.false" "expect(${1:value}).to.not.be.false;$0\n" "expect.to.not.be.false" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-be-false.yasnippet" nil "8476a6b9-f2c6-4b43-923f-512f8d1ffcbb")
                       ("expect.to.not.be.empty" "expect(${1:value}).to.not.be.empty;$0\n" "expect.to.not.be.empty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-not-be-empty.yasnippet" nil "23702711-b527-4a9b-bf30-55fa5965f3c8")
                       ("expect.to.match" "expect(${1:value}).to.match(/${2:regex}/);$0\n" "expect.to.match" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-match.yasnippet" nil "a9e38b3c-d1bb-4bd3-ab98-1f6582d3c6b0")
                       ("expect.to.include" "expect(${1:value}).to.include(${2:value});$0\n" "expect.to.include" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-include.yasnippet" nil "92652ce9-a07f-446a-801b-98f58ae11262")
                       ("expect.to.include.keys" "expect(${1:value}).to.include.keys(\"${2:string}\");$0\n" "expect.to.include.keys" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-include-keys.yasnippet" nil "c13504f8-291f-4029-a4b3-07648dd2b04f")
                       ("expect.to.have.string" "expect(${1:value}).to.have.string(\"${2:string}\");$0\n" "expect.to.have.string" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-string.yasnippet" nil "04cf3af0-4668-44c5-b270-473f18c2c02b")
                       ("expect.to.have.property" "expect(${1:value}).to.have.property(\"${2:value}\");$0\n" "expect.to.have.property" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-property.yasnippet" nil "89b2fee0-cf36-43b4-b143-754d06621784")
                       ("expect.to.have.ownProperty" "expect(${1:value}).to.have.ownProperty(\"${2:value}\");$0\n" "expect.to.have.ownProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-own-property.yasnippet" nil "6b247a4c-5bde-40f5-bbaf-9b8b038dc2ee")
                       ("expect.to.have.length" "expect(${1:value}).to.have.length(${2:value});$0\n" "expect.to.have.length" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-length.yasnippet" nil "396e5e9c-d909-4372-8d5e-1d65a66f42ab")
                       ("expect.to.have.length.within" "expect(${1:value}).to.have.length.within(${2:start}, ${3:finish});$0\n" "expect.to.have.length.within" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-length-within.yasnippet" nil "1bc7deec-14d7-40f2-96ea-284a491cd54b")
                       ("expect.to.have.length.of.at.most" "expect(${1:value}).to.have.length.of.at.most(${2:value});$0\n" "expect.to.have.length.of.at.most" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-length-of-at-most.yasnippet" nil "2125c77d-60fb-4463-906e-0a88066f568e")
                       ("expect.to.have.length.below" "expect(${1:value}).to.have.length.below(${2:value});$0\n" "expect.to.have.length.below" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-length-below.yasnippet" nil "33085940-1b9d-44cc-9406-f076834732d5")
                       ("expect.to.have.length.above" "expect(${1:value}).to.have.length.above(${2:value});$0\n" "expect.to.have.length.above" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-length-above.yasnippet" nil "816b973b-fec5-49f5-91a9-bc84460a788c")
                       ("expect.to.have.keys" "expect(${1:value}).to.have.keys(\"${2:string}\");$0\n" "expect.to.have.keys" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-keys.yasnippet" nil "4f7c0bd2-dfe8-47a7-8041-be2b1daf0a8a")
                       ("expect.to.have.deep.property" "expect(${1:value}).to.have.deep.property(\"${2:value}\");$0\n" "expect.to.have.deep.property" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-have-deep-property.yasnippet" nil "a77c48db-d4f7-44fb-96cc-379cd50aa4d2")
                       ("expect.to.exist" "expect(${1:value}).to.exist;$0\n" "expect.to.exist" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-exist.yasnippet" nil "a3bf6014-7914-4a04-afa2-5db21a8c6ceb")
                       ("expect.to.equal" "expect(${1:value}).to.equal(${2:value});$0\n" "expect.to.equal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-equal.yasnippet" nil "ce244efb-9688-4810-8b53-66ce1c31363a")
                       ("expect.to.eql" "expect(${1:value}).to.eql(${2:value});$0\n" "expect.to.eql" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-eql.yasnippet" nil "8060ba60-9443-474c-a317-a205af984505")
                       ("expect.to.contain" "expect(${1:value}).to.contain(\"${2:string}\");$0\n" "expect.to.contain" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-contain.yasnippet" nil "0a2670fe-9bd3-4e16-8f9a-c61d2a2a15f9")
                       ("expect.to.be.within" "expect(${1:value}).to.be.within(${2:start}, ${3:finish});$0\n" "expect.to.be.within" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-within.yasnippet" nil "348839ea-bc57-42aa-97ea-eb8fb13d5489")
                       ("expect.to.be.undefined" "expect(${1:value}).to.be.undefined;$0\n" "expect.to.be.undefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-undefined.yasnippet" nil "050ad30c-8830-4ecb-9a28-ec6633d897e3")
                       ("expect.to.be.true" "expect(${1:value}).to.be.true;$0\n" "expect.to.be.true" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-true.yasnippet" nil "b458ec35-ee68-498f-a4c2-cec70a4a2705")
                       ("expect.to.be.ok" "expect(${1:value}).to.be.ok;$0\n" "expect.to.be.ok" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-ok.yasnippet" nil "8a5d6f5c-6ce2-465f-84ef-869536adcfa9")
                       ("expect.to.be.null" "expect(${1:value}).to.be.null;$0\n" "expect.to.be.null" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-null.yasnippet" nil "e1f608c2-f7b7-438d-9796-474d2ba76a04")
                       ("expect.to.be.false" "expect(${1:value}).to.be.false;$0\n" "expect.to.be.false" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-false.yasnippet" nil "6e8c9478-b40a-410f-812a-9e16ff25a9b3")
                       ("expect.to.be.empty" "expect(${1:value}).to.be.empty;$0\n" "expect.to.be.empty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-empty.yasnippet" nil "6cc0fbc2-2f0e-4e0a-a1e3-05b0d5b9cf5f")
                       ("expect.to.be.closeTo" "expect(${1:value}).to.be.closeTo(${2:expected}, ${3:delta});$0\n" "expect.to.be.closeTo" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-close-to.yasnippet" nil "eb3516b1-07db-4d34-8e29-42d486825b16")
                       ("expect.to.be.below" "expect(${1:value}).to.be.below(${2:value});$0\n" "expect.to.be.below" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-below.yasnippet" nil "2122dfe7-631e-4717-9a13-a933d471645c")
                       ("expect.to.be.at.most" "expect(${1:value}).to.be.at.most(${2:value});$0\n" "expect.to.be.at.most" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-at-most.yasnippet" nil "05c77e5f-fabb-4e7e-8db8-810899fb89ce")
                       ("expect.to.be.at.least" "expect(${1:value}).to.be.at.least(${2:value});$0\n" "expect.to.be.at.least" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-at-least.yasnippet" nil "e9d97ea3-0b4b-435e-841e-7caf32a89b4d")
                       ("expect.to.be.arguments" "expect(${1:value}).to.be.arguments(${2:value});$0\n" "expect.to.be.arguments" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-arguments.yasnippet" nil "34fd5935-d67c-4697-9848-f80143d2113f")
                       ("expect.to.be.an" "expect(${1:value}).to.be.an(\"${2:type}\");$0\n" "expect.to.be.an" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-an.yasnippet" nil "5a219b53-8f48-460b-a586-5204c112c6ae")
                       ("expect.to.be.an.instanceof" "expect(${1:value}).to.be.an.instanceof(${2:object});$0\n" "expect.to.be.an.instanceof" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-an-instanceof.yasnippet" nil "9413136a-3f9b-44e6-ab34-5ce19dde7341")
                       ("expect.to.be.above" "expect(${1:value}).to.be.above(${2:value});$0\n" "expect.to.be.above" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-above.yasnippet" nil "9a49c771-b3f4-4082-b309-c5dd9a5ee2f7")
                       ("expect.to.be.a" "expect(${1:value}).to.be.a(\"${2:type}\");$0\n" "expect.to.be.a" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/expect-to-be-a.yasnippet" nil "2569b224-e3bf-482d-8330-df8e41fe256c")
                       ("assert.typeOf" "assert.typeOf(${1:value}, ${2:name}${3:, \"${4:[message]}\"});$0\n" "assert.typeOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.typeOf.yasnippet" nil "626c8b90-1c23-47bd-ba37-2c4b12b59c77")
                       ("assert.throws" "assert.throws(${1:function}, ${2:constructor/regexp}${3:, \"${4:[message]}\"});$0\n" "assert.throws" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.throws.yasnippet" nil "0abce9ac-df6d-40b6-8198-17493e27660c")
                       ("assert.strictEqual" "assert.strictEqual(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.strictEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.strictEqual.yasnippet" nil "5efd30f6-d10c-4994-8d5a-6e716fe1aaa9")
                       ("assert.propertyVal" "assert.propertyVal(${1:object}, ${2:property}, ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.propertyVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.propertyVal.yasnippet" nil "070ab30b-833f-4eff-a542-fb5112565ad2")
                       ("assert.propertyNotVal" "assert.propertyNotVal(${1:object}, ${2:property} ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.propertyNotVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.propertyNotVal.yasnippet" nil "272047bc-2aa5-49c0-b618-97aed787a342")
                       ("assert.property" "assert.property(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.property" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.property.yasnippet" nil "fa7b508b-e0fd-4c4b-89a9-e46b5cd1d2ad")
                       ("assert.operator" "assert.operator(${1:val1}, ${2:operator}, ${3:val2}${4:, \"${5:[message]}\"});$0\n" "assert.operator" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.operator.yasnippet" nil "82880423-fe15-4431-bb4b-272d8b13f6a4")
                       ("assert.ok" "assert.ok(${1:object}${2:, \"${3:[message]}\"});$0\n" "assert.ok" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.ok.yasnippet" nil "d269ccde-80a7-4926-b8ae-3648b04d8a66")
                       ("assert.notTypeOf" "assert.notTypeOf(${1:value}, ${2:name}${3:, \"${4:[message]}\"});$0\n" "assert.notTypeOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notTypeOf.yasnippet" nil "120cea89-b5b1-4230-b37e-fb147ac261e8")
                       ("assert.notStrictEqual" "assert.notStrictEqual(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.notStrictEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notStrictEqual.yasnippet" nil "f91c8b49-e7eb-4534-9ee7-6573f64068fc")
                       ("assert.notProperty" "assert.notProperty(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.notProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notProperty.yasnippet" nil "02557e70-a4f1-4ca5-8228-4a1e78af4a2d")
                       ("assert.notMatch" "assert.notMatch(${1:value}, ${2:regexp}${3:, \"${4:[message]}\"});$0\n" "assert.notMatch" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notMatch.yasnippet" nil "c5ef7a0d-f29c-4692-b268-d4acb4ab8e02")
                       ("assert.notInstanceOf" "assert.notInstanceOf(${1:object}, ${2:constructor}${3:, \"${4:[message]}\"});$0\n" "assert.notInstanceOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notInstanceOf.yasnippet" nil "da221867-bc6f-4874-8205-c56b5811005e")
                       ("assert.notEqual" "assert.notEqual(${1:actual}, ${2:expected}${3:, \"${3:[message]}\"});$0\n" "assert.notEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notEqual.yasnippet" nil "7a4c3523-5ca3-4b07-975f-e311f2293458")
                       ("assert.notDeepProperty" "assert.notDeepProperty(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.notDeepProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notDeepProperty.yasnippet" nil "3213e2f3-c2b7-41ca-88de-fa3324d95d11")
                       ("assert.notDeepEqual" "assert.notDeepEqual(${1:actual}, ${2:expected}${3:, \"${3:[message]}\"});$0\n" "assert.notDeepEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.notDeepEqual.yasnippet" nil "ea09b20e-fefa-476b-82a0-7d672a507dbf")
                       ("assert.match" "assert.match(${1:value}, ${2:regexp}${3:, \"${4:[message]}\"});$0\n" "assert.match" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.match.yasnippet" nil "fdb8692f-0536-438f-8808-8f16b6281dac")
                       ("assert.lengthOf" "assert.lengthOf(${1:object}, ${2:length}${3:, \"${4:[message]}\"});$0\n" "assert.lengthOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.lengthOf.yasnippet" nil "48326008-00ad-4863-afa8-74e9b0fe783a")
                       ("assert.isUndefined" "assert.isUndefined(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isUndefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isUndefined.yasnippet" nil "0b0aae04-6004-4f3d-9eb1-c3d578345ac3")
                       ("assert.isTrue" "assert.isTrue(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isTrue" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isTrue.yasnippet" nil "a86c0bb0-d7d2-4baf-adab-8507acb64f78")
                       ("assert.isString" "assert.isString(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isString" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isString.yasnippet" nil "aef44e96-ae55-4570-9953-3f407939adb2")
                       ("assert.isObject" "assert.isObject(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isObject" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isObject.yasnippet" nil "e4ffe78e-d76c-482e-b826-6c3b107d32d6")
                       ("assert.isNumber" "assert.isNumber(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNumber" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNumber.yasnippet" nil "ff111495-e0d7-4ecd-915a-870b5c348a93")
                       ("assert.isNull" "assert.isNull(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNull" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNull.yasnippet" nil "2d317b68-8f7c-4774-9e39-9ea29e3d2c47")
                       ("assert.isNotString" "assert.isNotString(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotString" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNotString.yasnippet" nil "4fc0b885-d3f0-4072-8087-f545b31cbc19")
                       ("assert.isNotObject" "assert.isNotObject(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotObject" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNotObject.yasnippet" nil "f73f70b1-cc59-4e8a-acad-32dbb47ae0bd")
                       ("assert.isNotNumber" "assert.isNotNumber(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotNumber" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNotNumber.yasnippet" nil "b5ed424c-1ba9-4fcc-8606-5b0d38f0ffe1")
                       ("assert.isNotNull" "assert.isNotNull(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotNull" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNotNull.yasnippet" nil "9bec4faa-b720-4925-b7d9-6d64b5b3b58d")
                       ("assert.isNotFunction" "assert.isNotFunction(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotFunction" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNotFunction.yasnippet" nil "241e8bc3-d631-41b7-a9eb-15b9089fa861")
                       ("assert.isNotBoolean" "assert.isNotBoolean(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotBoolean" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNotBoolean.yasnippet" nil "2dba6c56-208d-4c50-9994-a42919a5fb40")
                       ("assert.isNotArray" "assert.isNotArray(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotArray" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isNotArray.yasnippet" nil "584e4661-6829-46d9-a8e4-6d6997954ffa")
                       ("assert.isFunction" "assert.isFunction(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isFunction" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isFunction.yasnippet" nil "8ea6285f-4d0a-4843-8ed7-c80b3d9c2e27")
                       ("assert.isFalse" "assert.isFalse(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isFalse" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isFalse.yasnippet" nil "cdfda649-510c-45df-809f-25c0134cb2d6")
                       ("assert.isDefined" "assert.isDefined(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isDefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isDefined.yasnippet" nil "e18e3593-7869-44b9-8aa8-9cfdd15fdb23")
                       ("assert.isBoolean" "assert.isBoolean(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isBoolean" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isBoolean.yasnippet" nil "57e188da-cf2d-4137-a73e-577b2fe60a90")
                       ("assert.isArray" "assert.isArray(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isArray" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.isArray.yasnippet" nil "29e1184e-a8c9-4c76-9386-1b222baf55a7")
                       ("assert.instanceOf" "assert.instanceOf(${1:object}, ${2:constructor}${3:, \"${4:[message]}\"});$0\n" "assert.instanceOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.instanceOf.yasnippet" nil "da9c2d09-a0d1-4b09-a75c-c084aa412abd")
                       ("assert.include" "assert.include(${1:haystack}, ${2:needle}${3:, \"${4:[message]}\"});$0\n" "assert.include" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.include.yasnippet" nil "5ce1406b-828c-47fd-bbc1-9ac7be8fa375")
                       ("assert.fail" "assert.fail(${1:actual}, ${2:expected}${3:, \"${4:[message]}\", ${5:[operator]}});$0\n" "assert.fail" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.fail.yasnippet" nil "3c4e49c6-e96b-452a-a473-533c1ddcce9f")
                       ("assert.equal" "assert.equal(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.equal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.equal.yasnippet" nil "cdbe7853-e498-4c74-806b-54f206b9f40d")
                       ("assert.doesNotThrow" "assert.doesNotThrow(${1:function}, ${2:constructor/regexp}${3:, \"${4:[message]}\"});$0\n" "assert.doesNotThrow" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.doesNotThrow.yasnippet" nil "ade62988-5eb5-4621-9b85-f84ff277ce46")
                       ("assert.deepPropertyVal" "assert.deepPropertyVal(${1:object}, ${2:property} ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.deepPropertyVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.deepPropertyVal.yasnippet" nil "45498e01-401a-40f7-aba4-2b933290dd3f")
                       ("assert.deepPropertyNotVal" "assert.deepPropertyNotVal(${1:object}, ${2:property}, ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.deepPropertyNotVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.deepPropertyNotVal.yasnippet" nil "96c7c84f-66d2-4f1c-b8d5-19dc894f0630")
                       ("assert.deepProperty" "assert.deepProperty(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.deepProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.deepProperty.yasnippet" nil "bf1ca484-ccee-4abd-a5b7-f3cdfaf729e4")
                       ("assert.deepEqual" "assert.deepEqual(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.deepEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.deepEqual.yasnippet" nil "4c2ba0c3-0700-4b1d-8384-266f1ec1c295")
                       ("assert.closeTo" "assert.closeTo(${1:actual}, ${2:expected}, ${3:delta}${4:, \"${5:[message]}\"}$0\n" "assert.closeTo" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert.closeTo.yasnippet" nil "40d05976-2593-4789-b322-a8e017b492c0")
                       ("assert.typeOf" "assert.typeOf(${1:value}, ${2:name}${3:, \"${4:[message]}\"});$0\n" "assert.typeOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-type-of.yasnippet" nil "10c871ca-06b3-4105-ad85-26997414bfd8")
                       ("assert.throws" "assert.throws(${1:function}, ${2:constructor/regexp}${3:, \"${4:[message]}\"});$0\n" "assert.throws" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-throws.yasnippet" nil "f4d13b8c-14e3-4658-b082-f06b205c668a")
                       ("assert.strictEqual" "assert.strictEqual(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.strictEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-strict-equal.yasnippet" nil "750a793a-ff40-47b2-a7d5-6168f4e42c79")
                       ("assert.property" "assert.property(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.property" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-property.yasnippet" nil "81d2089b-4a08-4491-a29b-7915bec5faa7")
                       ("assert.propertyVal" "assert.propertyVal(${1:object}, ${2:property}, ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.propertyVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-property-val.yasnippet" nil "d6660ff0-2b51-40d5-a3fb-5188d291a3d3")
                       ("assert.propertyNotVal" "assert.propertyNotVal(${1:object}, ${2:property} ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.propertyNotVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-property-not-val.yasnippet" nil "60d7f82d-2003-4ae5-bf3e-895e7974d989")
                       ("assert.operator" "assert.operator(${1:val1}, ${2:operator}, ${3:val2}${4:, \"${5:[message]}\"});$0\n" "assert.operator" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-operator.yasnippet" nil "5c5f07e0-3b99-4c30-ab93-ebf6f41e1e19")
                       ("assert.ok" "assert.ok(${1:object}${2:, \"${3:[message]}\"});$0\n" "assert.ok" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-ok.yasnippet" nil "5287ba36-efd5-4e02-90a9-cc4e6605c06a")
                       ("assert.notTypeOf" "assert.notTypeOf(${1:value}, ${2:name}${3:, \"${4:[message]}\"});$0\n" "assert.notTypeOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-type-of.yasnippet" nil "7143e26f-4495-450f-ac30-9df11204168c")
                       ("assert.notStrictEqual" "assert.notStrictEqual(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.notStrictEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-strict-equal.yasnippet" nil "ae81b497-2753-4a95-9e64-8a35aee1a634")
                       ("assert.notProperty" "assert.notProperty(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.notProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-property.yasnippet" nil "deca27fe-73bc-4827-904e-27f162ab9720")
                       ("assert.notMatch" "assert.notMatch(${1:value}, ${2:regexp}${3:, \"${4:[message]}\"});$0\n" "assert.notMatch" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-match.yasnippet" nil "c3c0d990-7f48-4885-a767-003c12a776d1")
                       ("assert.notInstanceOf" "assert.notInstanceOf(${1:object}, ${2:constructor}${3:, \"${4:[message]}\"});$0\n" "assert.notInstanceOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-instance-of.yasnippet" nil "57389614-6f76-4fac-960c-55982f66e5c8")
                       ("assert.notEqual" "assert.notEqual(${1:actual}, ${2:expected}${3:, \"${3:[message]}\"});$0\n" "assert.notEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-equal.yasnippet" nil "167b8b46-2b91-409c-85bc-1b7adf4873b6")
                       ("assert.notDeepProperty" "assert.notDeepProperty(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.notDeepProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-deep-property.yasnippet" nil "49fe8293-388b-4f8c-84ea-ed2cb688da4d")
                       ("assert.notDeepEqual" "assert.notDeepEqual(${1:actual}, ${2:expected}${3:, \"${3:[message]}\"});$0\n" "assert.notDeepEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-not-deep-equal.yasnippet" nil "54ad84a4-9d16-4155-995e-49eae8f45369")
                       ("assert.match" "assert.match(${1:value}, ${2:regexp}${3:, \"${4:[message]}\"});$0\n" "assert.match" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-match.yasnippet" nil "5acaf18f-6f92-4565-a97e-56a290a1ecbc")
                       ("assert.lengthOf" "assert.lengthOf(${1:object}, ${2:length}${3:, \"${4:[message]}\"});$0\n" "assert.lengthOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-length-of.yasnippet" nil "78b3fbea-fa6c-4637-8616-07080494e3d4")
                       ("assert.isUndefined" "assert.isUndefined(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isUndefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-undefined.yasnippet" nil "ce3a4e2d-5304-4628-83c5-1c8825106725")
                       ("assert.isTrue" "assert.isTrue(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isTrue" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-true.yasnippet" nil "1daa23e2-885c-4399-b49f-2e82d8fb05c4")
                       ("assert.isString" "assert.isString(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isString" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-string.yasnippet" nil "9762d2df-1d23-4173-8dda-5fcb80a49c59")
                       ("assert.isObject" "assert.isObject(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isObject" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-object.yasnippet" nil "fdb3a257-0a70-4160-8506-7b12215a06ef")
                       ("assert.isNumber" "assert.isNumber(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNumber" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-number.yasnippet" nil "4662f9b6-7a0f-48fa-9075-3c4360260f3c")
                       ("assert.isNull" "assert.isNull(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNull" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-null.yasnippet" nil "d0bccf3d-2662-48fd-a974-807692fa56f8")
                       ("assert.isNotString" "assert.isNotString(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotString" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-not-string.yasnippet" nil "d4284ae9-af90-4eae-b6c0-0b60de80657e")
                       ("assert.isNotObject" "assert.isNotObject(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotObject" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-not-object.yasnippet" nil "c0f364cf-1dbf-4b7a-8e84-c7ede4bc69e9")
                       ("assert.isNotNumber" "assert.isNotNumber(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotNumber" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-not-number.yasnippet" nil "12caf64e-6cb7-48e3-b154-63d22cfb8438")
                       ("assert.isNotNull" "assert.isNotNull(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotNull" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-not-null.yasnippet" nil "7c594c58-82c9-4fda-b10f-5dc1615d07c6")
                       ("assert.isNotFunction" "assert.isNotFunction(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotFunction" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-not-function.yasnippet" nil "4672c525-a43a-48bd-b788-f4d2e91aaf09")
                       ("assert.isNotBoolean" "assert.isNotBoolean(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotBoolean" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-not-boolean.yasnippet" nil "ea5395c6-19e2-48fe-8341-c4da096d1b99")
                       ("assert.isNotArray" "assert.isNotArray(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isNotArray" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-not-array.yasnippet" nil "06f78b9a-e22f-4605-9a96-676821c4bd91")
                       ("assert.isFunction" "assert.isFunction(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isFunction" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-function.yasnippet" nil "eaaa4d8e-ab88-4d28-9a5d-a012d30ac10d")
                       ("assert.isFalse" "assert.isFalse(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isFalse" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-false.yasnippet" nil "bce55404-dcf4-4375-8668-fb9b75f584c4")
                       ("assert.isDefined" "assert.isDefined(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isDefined" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-defined.yasnippet" nil "74325450-04d7-4f3c-8abd-d2934cb44b6e")
                       ("assert.isBoolean" "assert.isBoolean(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isBoolean" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-boolean.yasnippet" nil "43a22ee8-4e8a-4fb8-b292-4ddd5edca669")
                       ("assert.isArray" "assert.isArray(${1:value}${2:, \"${3:[message]}\"});$0\n" "assert.isArray" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-is-array.yasnippet" nil "1a33b449-64e2-4245-a4b8-1839bc94fed7")
                       ("assert.instanceOf" "assert.instanceOf(${1:object}, ${2:constructor}${3:, \"${4:[message]}\"});$0\n" "assert.instanceOf" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-instance-of.yasnippet" nil "86c8f58d-c564-4baa-8107-9a581265c6ae")
                       ("assert.include" "assert.include(${1:haystack}, ${2:needle}${3:, \"${4:[message]}\"});$0\n" "assert.include" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-include.yasnippet" nil "ca9d75fe-1481-4449-8913-f0fa5ed53963")
                       ("assert.fail" "assert.fail(${1:actual}, ${2:expected}${3:, \"${4:[message]}\", ${5:[operator]}});$0\n" "assert.fail" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-fail.yasnippet" nil "eb3de7c1-7ee8-46c0-9a2e-bac40fb1977b")
                       ("assert.equal" "assert.equal(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.equal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-equal.yasnippet" nil "bea3eeff-b068-4ee3-9a9a-8feb84d42afe")
                       ("assert.doesNotThrow" "assert.doesNotThrow(${1:function}, ${2:constructor/regexp}${3:, \"${4:[message]}\"});$0\n" "assert.doesNotThrow" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-does-not-throw.yasnippet" nil "d3fcba19-0310-43dc-b938-1069e4336c78")
                       ("assert.deepProperty" "assert.deepProperty(${1:object}, ${2:property}${3:, \"${4:[message]}\"});$0\n" "assert.deepProperty" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-deep-property.yasnippet" nil "6d261cef-03e4-4708-b28c-b8090a2421e9")
                       ("assert.deepPropertyVal" "assert.deepPropertyVal(${1:object}, ${2:property} ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.deepPropertyVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-deep-property-val.yasnippet" nil "4d9f0359-c123-4ff2-9803-d2b83c7b3acb")
                       ("assert.deepPropertyNotVal" "assert.deepPropertyNotVal(${1:object}, ${2:property}, ${3:value}${4:, \"${5:[message]}\"});$0\n" "assert.deepPropertyNotVal" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-deep-property-not-val.yasnippet" nil "4cc534f6-f120-4953-9b3f-d45706b0d851")
                       ("assert.deepEqual" "assert.deepEqual(${1:actual}, ${2:expected}${3:, \"${4:[message]}\"});$0\n" "assert.deepEqual" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-deep-equal.yasnippet" nil "0a257afa-bbc7-4932-acc6-debe8fc1dac0")
                       ("assert.closeTo" "assert.closeTo(${1:actual}, ${2:expected}, ${3:delta}${4:, \"${5:[message]}\"}$0\n" "assert.closeTo" nil
                        ("chai")
                        nil "/Users/wyuenho/.emacs.d/snippets/js-mode/chai/assert-close-to.yasnippet" nil "5e58708a-3b91-4278-a840-b393961d88d2")))


;;; Do not edit! File generated at Thu Apr 26 04:40:03 2018
