(in-package :concord)

(defvar *ideographic-structure-feature-hash*
  (make-hash-table :test 'equal))

(defvar shuowen-radicals
 #(#\一 #\上 #\示 #\三 #\王 #\玉 #\玨 #\气 #\士 #\丨 ; 010
   #\屮 #\艸 #\蓐 #\茻 #\小 #\八 #\釆 #\半 #\牛 #\犛 ; 020
   #\告 #\口 #\凵 #\吅 #\哭 #\走 #\止 #\癶 #\步 #\此 ; 030
   #\正 #\是 #\辵 #\彳 #\廴 #\㢟 #\行 #\齒 #\牙 #\足 ; 040
   #\疋 #\品 #\龠 #\冊 #\㗊 #\舌 #\干 #\𧮫 #\只 #\㕯 ; 050
   #\句 #\丩 #\古 #\十 #\卅 #\言 #\誩 #\音 #\䇂 #\丵 ; 060
   #\菐 #\𠬞 #\𠬜 #\共 #\異 #\舁 #\𦥑 #\䢅 #\爨 #\革 ; 070
   #\鬲 #\䰜 #\爪 #\𠃨 #\鬥 #\又 #\𠂇 #\史 #\支 #\𦘒 ; 080
   #\聿 #\畫 #\隶 #\臤 #\臣 #\殳 #\殺 #\𠘧 #\寸 #\皮 ; 090
   #\㼱 #\攴 #\敎 #\卜 #\用 #\爻 #\㸚 #\𥄎 #\目 #\䀠 ; 100
   #\眉 #\盾 #\自 #\白 #\鼻 #\皕 #\習 #\羽 #\隹 #\奞 ; 110
   #\雈 #\𦫳 #\𥄕 #\羊 #\羴 #\瞿 #\雔 #\雥 #\鳥 #\烏 ; 120
   #\𠦒 #\冓 #\幺 #\𢆶 #\叀 #\玄 #\予 #\放 #\𠬪 #\𣦼 ; 130
   #\歺 #\死 #\冎 #\骨 #\肉 #\筋 #\刀 #\刃 #\㓞 #\丯 ; 140
   #\耒 #\角 #\竹 #\箕 #\丌 #\左 #\工 #\㠭 #\巫 #\甘 ; 150
   #\曰 #\乃 #\丂 #\可 #\兮 #\号 #\亏 #\旨 #\喜 #\壴 ; 160
;  #\旨 #\曰 #\乃 #\丂 #\可 #\兮 #\号 #\亏 #\喜 #\壴 ; 160
   #\鼓 #\豈 #\豆 #\豊 #\豐 #\䖒 #\虍 #\虎 #\虤 #\皿 ; 170
   #\𠙴 #\去 #\血 #\丶 #\丹 #\青 #\井 #\皀 #\鬯 #\食 ; 180
   #\亼 #\會 #\倉 #\入 #\缶 #\矢 #\高 #\冂 #\𩫏 #\京 ; 190
   #\亯 #\𣆪 #\畗 #\㐭 #\嗇 #\來 #\麥 #\夊 #\舛 #\䑞 ; 200
   #\韋 #\弟 #\夂 #\久 #\桀 #\木 #\東 #\林 #\才 #\叒 ; 210
   #\之 #\帀 #\出 #\𣎵 #\生 #\乇 #\𠂹 #\𠌶 #\華 #\𥝌 ; 220
   #\稽 #\巢 #\桼 #\束 #\㯻 #\囗 #\員 #\貝 #\邑 #\𨛜 ; 230
   #\日 #\旦 #\倝 #\㫃 #\冥 #\晶 #\月 #\有 #\明 #\囧 ; 240
   #\夕 #\多 #\毌 #\𢎘 #\𣐺 #\卣 #\齊 #\朿 #\片 #\鼎 ; 250
   #\克 #\彔 #\禾 #\秝 #\黍 #\香 #\米 #\毇 #\臼 #\凶 ; 260
   #\𣎳 #\𣏟 #\麻 #\尗 #\耑 #\韭 #\瓜 #\瓠 #\宀 #\宮 ; 270
   #\呂 #\穴 #\㝱 #\疒 #\冖 #\𠔼 #\冃 #\㒳 #\网 #\襾 ; 280
   #\巾 #\巿 #\帛 #\白 #\㡀 #\黹 #\人 #\𠤎 #\匕 #\从 ; 290
   #\比 #\北 #\丘 #\㐺 #\𡈼 #\重 #\臥 #\身 #\㐆 #\衣 ; 300
   #\裘 #\老 #\毛 #\毳 #\尸 #\尺 #\尾 #\履 #\舟 #\方 ; 310
   #\儿 #\兄 #\兂 #\皃 #\𠑹 #\先 #\秃 #\見 #\覞 #\欠 ; 320
   #\㱃 #\㳄 #\旡 #\頁 #\𦣻 #\面 #\丏 #\首 #\𥄉 #\須 ; 330
   #\彡 #\彣 #\文 #\髟 #\后 #\司 #\卮 #\卩 #\印 #\色 ; 340
   #\𠨍 #\辟 #\勹 #\包 #\茍 #\鬼 #\甶 #\厶 #\嵬 #\山 ; 350
   #\屾 #\屵 #\广 #\厂 #\丸 #\危 #\石 #\長 #\勿 #\冄 ; 360
   #\而 #\豕 #\㣇 #\彑 #\豚 #\豸 #\𤉡 #\易 #\象 #\馬 ; 370
   #\𢊁 #\鹿 #\麤 #\㲋 #\兔 #\萈 #\犬 #\㹜 #\鼠 #\能 ; 380
   #\熊 #\火 #\炎 #\黑 #\囪 #\焱 #\炙 #\赤 #\大 #\亦 ; 390
   #\夨 #\夭 #\交 #\尣 #\壺 #\壹 #\㚔 #\奢 #\亢 #\夲 ; 400
   #\夰 #\亣 #\夫 #\立 #\竝 #\囟 #\思 #\心 #\惢 #\水 ; 410
   #\沝 #\瀕 #\𡿨 #\巜 #\川 #\泉 #\灥 #\永 #\𠂢 #\谷 ; 420
   #\仌 #\雨 #\雲 #\魚 #\𩺰 #\燕 #\龍 #\飛 #\非 #\卂 ; 430
   #\𠃉 #\不 #\至 #\西 #\鹵 #\鹽 #\戶 #\門 #\耳 #\𦣝 ; 440
   #\手 #\𠦬 #\女 #\毋 #\民 #\丿 #\𠂆 #\乁 #\氏 #\氐 ; 450
   #\戈 #\戉 #\我 #\亅 #\琴 #\𠃊 #\亡 #\匸 #\匚 #\曲 ; 460
   #\甾 #\瓦 #\弓 #\弜 #\弦 #\系 #\糸 #\素 #\絲 #\率 ; 470
   #\虫 #\䖵 #\蟲 #\風 #\它 #\龜 #\黽 #\卵 #\二 #\土 ; 480
   #\垚 #\堇 #\里 #\田 #\畕 #\黃 #\男 #\力 #\劦 #\金 ; 490
   #\幵 #\勺 #\几 #\且 #\斤 #\斗 #\矛 #\車 #\𠂤 #\𨸏 ; 500
   #\𨺅 #\厽 #\四 #\宁 #\叕 #\亞 #\五 #\六 #\七 #\九 ; 510
   #\禸 #\嘼 #\甲 #\乙 #\丙 #\丁 #\戊 #\己 #\巴 #\庚 ; 520
   #\辛 #\辡 #\壬 #\癸 #\子 #\了 #\孨 #\𠫓 #\丑 #\寅 ; 530
   #\卯 #\辰 #\巳 #\午 #\未 #\申 #\酉 #\酋 #\戌 #\亥 ; 540
   ))
