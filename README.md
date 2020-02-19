
# twli

台灣里R套件

版本：0.01

### 簡介：

台灣約有7700多個村里，由於人口流動、行政區重新區劃，村里時常進行整併，村里整併問題造成跨年度村里分析、研究時的極大難題。台灣里套件將研究分析年度的村里整併情況進行彙整，讓使用者能根據彙整結果將各年度的村里相關數據合併後能保持一致，以利合併為各年度村里相同的面板數據（panel data）。

### 使用說明：


#### 1. 處理流程：

<p align="center"> 
<img src="https://github.com/torrentpien/twli/blob/master/%E7%A4%BA%E6%84%8F%E5%9C%96.png?raw=true">
</p>

  台灣里套件目前初步開發四個函數，分別為liRef、liSum、liEqu、liShp，四個函數的功能及與所需整併資料的關係如下：

  *  **台灣村里整併檔案**：按照台灣里套件規格編制的台灣村里整併檔案，使用者可按照自己的需求以csv、xlsx或其他檔案格式進行記錄。對台灣里套件使用者來說，台灣村里整併檔案為基礎資料表格，需不斷增補與更新，紀錄研究分析所需的村里合併或分割資料，透過其他台灣里函數，將村里整併檔案中的內容進行各種村里整併。台灣村里整併表編制規格請參考。

  *  **所需整併村里資料檔**：研究者需要整併的資料。需包含村里代碼欄位，建議使用內政部村里代碼，如桃園市桃園區文化里的村里代碼為：6800100-001。

  *  **liRef**函數：按照研究的年度範圍及所需整併的檔案年度，將台灣村里整併檔案整理後產生村里整併參照表。

  *  **村里整併參照表**：liRef函數產生的R數據框（dataframe），內容為所需整併村里的分組列表，以利liSum、liEqu、liShp等函數進行村里整併運算。

  *  **liSum**函數：需整併村里資料的相加。

  *  **liEqu**函數：許多需整併村里資料不是簡單的相加，需要各欄位運算後產生新數字，台灣里套件提供使用者自訂公式，計算村里整併後的新數據。

  *  **liShp**函數：將村里shapefile圖資按照村里整併參照檔進行整併。

         注意：liSum、liEqu、liShp三個村里整併運算函數會留一組需整併村里的一個村里（除了被合併的消滅村里，會留下村里編號最小的村里），並刪除同組中其他村里資料，並在留下的村里中以li_adject欄位標記"1"。

#### 2. 安裝：


#### 3. 台灣村里整併表編制規格
  
 1. 必要欄位：村里代碼、年度、該年度整併模式
  
    如果研究分析所記錄的村里整併資料年度為2014、2018年，必要欄位如下：

  | li_id | 2014 | mode_2014 | 2018 | mode_2018 |
  |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    | 
  | 村里代碼 | 2014執行村里整併之村里代碼 | 2014年整併模式 |  2018執行村里整併之村里代碼 | 2018年整併模式 |

 2. 必要欄位說明：
     
  *  li_id（村里代碼）：列出所有的村里代碼，包括因為合併後被消滅的村里。建議使用內政部村里代碼。

  *  2014、2018（各年度執行村里整併之村里代碼）：鍵入各年度執行村里整併之村里代碼，所謂執行村里整併之村里的定義為，在進行合併時，輸入最後完成合併的村里代碼，在進行分割時，則輸入進行分割的原始村里代碼。如果不只一個村里，以"|"分隔。

  *  mode_2014、mode_2018（各年度整併模式）：包括下列四種模式，請以英文小寫編碼
  
      *  m: merge，指村里合併，也就是說，li_id欄位的村里被消滅，於該年度合併進「最終村里代碼」的村里。

      *  s: separate，指村里分割，「最終村里代碼」的村里分割為li_id欄位的村里及「最終村里代碼」的村里。
      
      *  t: trim，指村里的部分修整，村里的部分居民，被移到其他村里。

      *  c: complex，指複雜型村里整併，複雜的定義為牽涉m、s、t裡兩種模式以上村里之合併及分割。

      *  vbox: vote box，指兩個村里被編為一個選舉投開票所，如果處理選舉資料，必須將其他沒有進行村裡整併的村里人口、教育等資料進行整併。

 3. 非必要欄位：  
  
  *  可在各年度執行村里整併之村里代碼前放進其他欄位，台灣村里整併表範例（li_table.xlsx）即按照下列欄位編制：

| city_code | city_name | district_code | district_name | li_id | li_name | li_id_dgb | 2014 | mode_2014 | 2018 | mode_2018 |
|    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |
| 縣市代碼 | 縣市名稱 | 區代碼 | 區名稱 | 村里代碼 | 村里名稱 | 主計處村里代碼 | 2014 | mode_2014 | 2018 | mode_2018 |
|    |    |    |    | ● |    |    | ● | ● | ● | ● |

     ●：必要欄位

     注意：各年度執行村里整併之村里代碼、各年度整併模式(即2014、mode_2014、2018、mode_2018)必須置於台灣村里整併檔案最後欄位。

 4. 整併模式輸入案例：

  *  m（合併）: 2014年7月，高雄市鳳山區海風里（6401200-023）因為人口過少，僅剩46戶，高雄市政府宣布將海風里合併進鳳山區海光里（6401200-022）。
  
| city_code | city_name | district_code | district_name | li_id | li_name | li_id_dgb | 2014 | mode_2014 | 2018 | mode_2018 |
|    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |
|  64  |  高雄市  |  6401200  |  鳳山區  |  6401200-023  |  海風里  |  64000121023  |   6401200-022  |  m  |  |  |

  *  s（分割）：2018年3月，桃園市桃園區忠義里（6800100-005）因人口增加，忠義里拆為忠義里及新增的福元里（6800100-079）。  

| city_code | city_name | district_code | district_name | li_id | li_name | li_id_dgb | 2014 | mode_2014 | 2018 | mode_2018 |
|    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |
|  68  |  桃園市  |  6800100  |  桃園區  |  6800100-079  |  福元里  |  68000010079  |   |  |  6800100-005  |  s  |

  *  t（修整）：2018年1月，台南市新營區民榮里（6700100-015）與南紙里（6700100-020）進行邊界調整，南紙里部分居民劃歸為民榮里。
  
| city_code | city_name | district_code | district_name | li_id | li_name | li_id_dgb | 2014 | mode_2014 | 2018 | mode_2018 |
|    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |
|  67  |  台南市  |  6700100  |  新營區  |  6700100-020  |  南紙里  |  67000010020  |  |  |  6700100-015  |  t  |
  

  *  c（複雜）：2014年1月，桃園市蘆竹區因人口增加，進行村里整併，其中中福里（6800500-002）、大竹里（6800500-004）、新興里（6800500-001）各取部分居民組為上興里（6800500-037），但同時，中福里（6800500-002）又將部分居民分割為中興里（6800500-036），又同時，富竹里（6800500-021）及上竹里（6800500-022）將部分居民劃歸為大竹里（6800500-004）。

| city_code | city_name | district_code | district_name | li_id | li_name | li_id_dgb | 2014 | mode_2014 | 2018 | mode_2018 |
|    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |
|  68  |  桃園市  |  6800500  |  蘆竹區  |  6800500-021  |  富竹里  |  68000050021  |   6800500-004  |  c  |  |  |
|  68  |  桃園市  |  6800500  |  蘆竹區  |  6800500-022  |  上竹里  |  68000050022  |   6800500-004  |  c  |  |  |
|  68  |  桃園市  |  6800500  |  蘆竹區  |  6800500-036  |  中興里  |  68000050036  |   6800500-002  |  c  |  |  |
|  68  |  桃園市  |  6800500  |  蘆竹區  |  6800500-037  |  上興里  |  68000050037  |   6800500-001\|6800500-002\|6800500-004  |  c  |  |  |

  *  vbox（一投開票所多村里）：2014年縣市長選舉，台中市西屯區協和里（6600600-020）、福恩里（6600600-035）部分居民合併至獨立投開票所（6600600-9999）投票。

| city_code | city_name | district_code | district_name | li_id | li_name | li_id_dgb | 2014 | mode_2014 | 2018 | mode_2018 |
|    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |    :---:    |
|  66  |  台中市  |  6600600  |  西屯區  |  6600600-035  |  福恩里  |  66000060035  |   6600600-020\|6600600-9999  |  vbox  |  |  |

#### 4. 函數說明：

 1. **liRef(x, range = c("a", "b"), year = "c", vbox = FALSE, done = TRUE)**

    *  用途：將R讀入的「台灣村里整併檔案」轉為「村里整併參照表」。

    *  說明：由於每個研究所需的時間範圍不同，時間範圍外的村里整併可以忽略不計，例如要研究2014-2018年的村里人口變遷，2020完成的村里整併就不用處理，但如果研究範圍擴大到2020年，2018-2020年的村里整併就要通盤考量納入計算，假設2020年A與B里整併為A里，2020年的村里資料的A里包含了B里的資料，2014-2018年A與B未合併時的村里資料，都要進行整併處理。

    *  **x**: 格式為dataframe，為R讀入的「台灣村里整併檔案」，檔案中必須有li_id欄位，year及mode_year欄位必須置於表格後段。

    *  **range**: 設定研究時間範圍，例如研究範圍為2005-2010，year_range = c("2005", "2010")。

    *  **year**: 「所需整併村里資料檔」的時間，單位為「年」。

    *  **vbox**: 研究計畫中是否有選舉檔案，預設為否（FALSE）。如果是要把村里人口資料合併選舉資料，在整併村里人口資料時，vbox也要設為TRUE。

    *  **done**: 「所需整併村里資料檔」完成時，是否完成該年度的村里整併，預設為是（TRUE）。有些村里資料是該年度3月完成，但村里整併卻發生在9月，這時done要設為FALSE。

 2. **liSum(x, ref = y, cols = c(a:b))**

    *  用途：對需整併村里資料進行加總。

    *  說明：將「村里整併參照表」中列出所需要進行整併的村里，按照指定欄位進行加總。

    *  **x**: 格式為dataframe，為R讀入的「所需整併村里資料檔」，檔案中必須要有li_id欄位。

    *  **ref**: liRef函數所產生「村里整併參照表」的物件名稱。

    *  **cols**: 「所需整併村里資料檔」中需要整併的欄位。

 3. **liEqu(x, ref = y, result = c(""), equ = c(""))**

    *  用途：對指定欄位設定公式，進行村里資料整併。

    *  說明：許多村里資料整併時無法進行直接加總，必須經過換算，liEqu函數能將「村里整併參照表」中列出所需要進行整併的村里，指定特定欄位放入公式後進行整併。如人口密度的整併，必須先加總村里的人口數及面積後再相除才能得出整併後的人口密度。

    *  **x**: 格式為dataframe，為R讀入的「所需整併村里資料檔」，檔案中必須要有li_id欄位。

    *  **ref**: liRef函數所產生「村里整併參照表」的物件名稱。

    *  **result**: 「所需整併村里資料檔」中完成整併的欄位，也就是公式等號的左側。

    *  **equ**: 計算公式，也就是公式等號的右側，需正確輸入「所需整併村里資料檔」中的欄位，輸入模式為"population / area"

 4. **liShp(x, ref = y)**

    *  用途：將村里shapefile圖資按照「村里整併參照檔」進行整併。

    *  說明：在村里資料完成整併後，需將GIS圖資的相同村里進行整併，以進行套疊或空間回歸。

    *  **x**: 格式為shapefile物件，為R讀入的GIS檔案，檔案的data中必須要有li_id欄位。

    *  **ref**: liRef函數所產生「村里整併參照表」的物件名稱。

### 版本說明

  *  0.01 (2020.2.13): 開發中


