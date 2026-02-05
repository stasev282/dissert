# Отчет по CFA / иерархической факторной модели (3 файла)

Ниже — связный трек того, **на каких табличных показателях** держатся решения по чистке шкал и финальной иерархической модели. Все значения берутся из Excel-листов; рядом с русским названием показателя я всегда указываю **имя столбца в таблице**.

---

## 1) Файл `cfa_initial.xlsx` — начальная CFA-модель (до чистки и до объединения 3 факторов)

### 1.1. Качество подгонки модели (лист `fit_measures`)
Показатели подгонки (выборка, `n` = **774**, столбец `n`):

- **CFI / сравнительный индекс подгонки** (`cfi`) = **0.992**
- **TLI / индекс Таккера–Льюиса** (`tli`) = **0.990**
- **RMSEA / среднеквадратичная ошибка аппроксимации** (`rmsea`) = **0.070**
- **RMSEA 95% ДИ нижняя/верхняя граница** (`rmsea_ci_lower`, `rmsea_ci_upper`) = **0.066 / 0.074**
- **SRMR / стандартизированный среднеквадратичный остаток** (`srmr`) = **0.050**

### 1.2. Надежность факторов (лист `reliability_alpha_cr`)
Здесь опора на:
- **альфа Кронбаха** (`cronbach_alpha`)
- **композитная надежность (CR)** (`composite_reliability`)

Таблица надежности:

| factor           |   cronbach_alpha |   composite_reliability |
|:-----------------|-----------------:|------------------------:|
| assortment       |            0.710 |                   0.800 |
| attitude         |            0.794 |                   0.849 |
| benefit          |            0.764 |                   0.834 |
| cannibalization  |            0.445 |                   0.454 |
| loyalty_item     |            0.833 |                   0.873 |
| loyalty_retailer |            0.799 |                   0.856 |
| retailer_img     |            0.837 |                   0.875 |
| stm              |            0.536 |                   0.598 |
| uniqueness       |            0.766 |                   0.821 |

Ключевой вывод из этой таблицы (без философии, чисто метрики):
- Факторы **`cannibalization`** и **`stm`** дают низкую **альфу Кронбаха** (`cronbach_alpha` = **0.445** и **0.536**) и низкую **CR** (`composite_reliability` = **0.454** и **0.598**) — это и есть количественная база для дальнейшей чистки.

### 1.3. Факторные нагрузки (лист `loadings`)
В таблице нагрузок используются столбцы:
- **фактор** (`factor`), **индикатор** (`indicator`)
- **стандартизованная нагрузка** (`loading_std`)
- **нестандартизованная нагрузка** (`loading_unstd`)
- **стандартная ошибка** (`se`), **z-статистика** (`z`), **p-value** (`pvalue`)

Полная таблица нагрузок (initial):

| factor           | indicator                                  |   loading_std |   loading_unstd |    se |      z | pvalue   |
|:----------------|:--------------------------------------------|--------------:|----------------:|------:|-------:|:--------|
| assortment      | assortment_item_category_coverage_likert    |         0.828 |           0.828 | 0.016 | 50.914 | 0       |
| assortment      | assortment_item_price_range_likert          |         0.748 |           0.748 | 0.017 | 43.404 | 0       |
| assortment      | assortment_item_wide_choice_likert          |         0.689 |           0.689 | 0.019 | 36.535 | 0       |
| attitude        | attitude_item_prefer_available_likert       |         0.876 |           0.876 | 0.012 | 74.299 | 0       |
| attitude        | attitude_item_brand_alternative_likert      |         0.840 |           0.840 | 0.014 | 60.283 | 0       |
| benefit         | benefit_item_value_money_likert             |         0.839 |           0.839 | 0.015 | 54.997 | 0       |
| benefit         | benefit_item_choose_same_price_likert       |         0.830 |           0.830 | 0.016 | 53.274 | 0       |
| benefit         | benefit_item_saves_money_likert             |         0.698 |           0.698 | 0.020 | 34.316 | 0       |
| cannibalization | cannibalization_item_low_price_likert       |         0.680 |           0.680 | 0.031 | 22.076 | 0       |
| cannibalization | cannibalization_item_alternative_likert     |         0.453 |           0.453 | 0.042 | 10.779 | 0       |
| cannibalization | cannibalization_item_not_good_likert        |         0.242 |           0.242 | 0.050 |  4.863 | 1.16e-06 |
| loyalty_item    | loyalty_item_buy_more_likert                |         0.851 |           0.851 | 0.013 | 64.495 | 0       |
| loyalty_item    | loyalty_item_recommend_likert               |         0.826 |           0.826 | 0.015 | 55.917 | 0       |
| loyalty_item    | loyalty_item_prefer_likert                  |         0.825 |           0.825 | 0.015 | 54.906 | 0       |
| loyalty_retailer| loyalty_retailer_choose_store_likert        |         0.892 |           0.892 | 0.012 | 77.194 | 0       |
| loyalty_retailer| loyalty_retailer_intend_continue_likert     |         0.816 |           0.816 | 0.015 | 55.410 | 0       |
| loyalty_retailer| loyalty_retailer_like_store_likert          |         0.727 |           0.727 | 0.020 | 35.898 | 0       |
| retailer_img    | retailer_brand_item_logo_quality_trust_likert|        0.886 |           0.886 | 0.011 | 80.800 | 0       |
| retailer_img    | retailer_brand_item_quality_guarantee_likert |        0.879 |           0.879 | 0.012 | 76.613 | 0       |
| stm             | stm_knowledge                               |         0.803 |           0.803 | 0.050 | 16.036 | 0       |
| stm             | stm_share                                   |         0.700 |           0.700 | 0.055 | 12.669 | 0       |
| stm             | stm_freq                                    |         0.153 |           0.153 | 0.058 |  2.632 | 0.0085  |
| uniqueness      | uniqueness_item_new_interest_likert         |         0.818 |           0.818 | 0.017 | 48.246 | 0       |
| uniqueness      | uniqueness_item_visit_for_pl_likert         |         0.796 |           0.796 | 0.019 | 41.741 | 0       |
| uniqueness      | uniqueness_item_unique_features_likert      |         0.716 |           0.716 | 0.022 | 32.861 | 0       |

Здесь видно, почему некоторые части модели были “кандидатами на вылет” именно по нагрузкам:
- У фактора **`stm`** есть индикатор `stm_freq` со **стандартизованной нагрузкой** (`loading_std`) = **0.153** (и значимость `pvalue` = **0.0085**), то есть измерительная связь слабая.
- У фактора **`cannibalization`** индикатор `cannibalization_item_not_good_likert` имеет `loading_std` = **0.242**.

### 1.4. Корреляции латентных факторов и причина объединения 3 факторов (лист `latent_correlations`)
Решение “схлопнуть” три фактора в один основано на высокой латентной корреляции (столбец **`corr`**) и содержательной совместимости.

Корреляции между тремя факторами (`assortment`, `benefit`, `uniqueness`):

| factor_1    | factor_2    |   corr |    se |      z | pvalue   |
|:------------|:------------|-------:|------:|-------:|:--------|
| assortment  | benefit     |  0.918 | 0.015 | 59.445 | 0       |
| benefit     | uniqueness  |  0.870 | 0.019 | 46.872 | 0       |
| assortment  | uniqueness  |  0.861 | 0.019 | 45.057 | 0       |

То есть в initial-модели три отдельных конструкта статистически ведут себя как очень близкие (по `corr` = **0.861–0.918**), поэтому дальше логично проверить, **насколько корректно они могут быть объяснены общим вторым порядком**.

### 1.5. Проверка распределений STM-переменных (лист `stm_distribution_check`)
Лист фиксирует “техническую гигиену” по STM: его смотреть не надо

---

## 2) Файл `cfa_second_order_loadings.xlsx` — тест второго порядка (можно ли собрать 3 фактора в один “двухэтажный”)

Этот файл отвечает на вопрос: **если ввести общий фактор 2-го порядка (`perceived_value`), будут ли 3 фактора 1-го порядка нормально на него загружаться?**

### 2.1. Качество подгонки (лист `fit_measures`)
Столбцы те же по смыслу + добавлены χ² и df:
- **n** (`n`) = **774**
- **χ² / хи-квадрат** (`chisq`) = **124.654**
- **степени свободы** (`df`) = **24**
- **CFI** (`cfi`) = **0.995**
- **TLI** (`tli`) = **0.992**
- **RMSEA** (`rmsea`) = **0.074** с ДИ (`rmsea_ci_lower`, `rmsea_ci_upper`) = **0.061 / 0.087**
- **SRMR** (`srmr`) = **0.040**
- `pvalue` для χ² в файле = **NA** (столбец `pvalue`)

### 2.2. Нагрузки второго порядка (лист `loadings_second_order`)
Смысл столбцов:
- **фактор 2-го порядка** (`factor`) = `perceived_value`
- **факторы 1-го порядка как индикаторы** (`indicator`) = `benefit`, `assortment`, `uniqueness`
- **стандартизованная нагрузка** (`loading_std`) + значимость (`p_value`)

| factor          | indicator   |   loading_std |   loading_unstd |    se |      z | p_value   |
|:----------------|:------------|--------------:|----------------:|------:|-------:|:---------|
| perceived_value | benefit     |         0.965 |           3.695 | 0.665 |  5.557 | 2.74e-08 |
| perceived_value | assortment  |         0.957 |           3.281 | 0.572 |  5.736 | 9.70e-09 |
| perceived_value | uniqueness  |         0.904 |           2.114 | 0.211 | 10.005 | 0        |

Итого по этому листу: все три фактора **значимо** и **сильно** загружаются на общий второй порядок (`loading_std` = **0.904–0.965**, `p_value` ≤ **2.74e-08**).

### 2.3. Нагрузки первого порядка (лист `loadings_first_order`)
Это подтверждает, что внутри каждого из трех факторов 1-го порядка индикаторы держатся нормально (столбцы `loading_std`, `p_value`).

| factor      | indicator                                  |   loading_std |   loading_unstd |    se |      z | p_value   |
|:------------|:--------------------------------------------|--------------:|----------------:|------:|-------:|:---------|
| assortment  | assortment_item_category_coverage_likert    |         0.806 |           0.235 | 0.039 |  5.952 | 2.65e-09 |
| assortment  | assortment_item_price_range_likert          |         0.756 |           0.220 | 0.036 |  6.169 | 6.85e-10 |
| assortment  | assortment_item_wide_choice_likert          |         0.703 |           0.205 | 0.033 |  6.270 | 3.61e-10 |
| benefit     | benefit_item_value_money_likert             |         0.863 |           0.225 | 0.038 |  5.891 | 3.84e-09 |
| benefit     | benefit_item_choose_same_price_likert       |         0.785 |           0.205 | 0.035 |  5.871 | 4.34e-09 |
| benefit     | benefit_item_saves_money_likert             |         0.717 |           0.187 | 0.031 |  6.005 | 1.91e-09 |
| uniqueness  | uniqueness_item_new_interest_likert         |         0.817 |           0.350 | 0.030 | 11.738 | 0        |
| uniqueness  | uniqueness_item_visit_for_pl_likert         |         0.780 |           0.334 | 0.027 | 12.161 | 0        |
| uniqueness  | uniqueness_item_unique_features_likert      |         0.735 |           0.314 | 0.026 | 11.868 | 0        |

---

## 3) Файл `cfa_hcm_loadings_reliability.xlsx` — итоговая иерархическая модель (HCM)

Это “финальная сборка”: очищенная структура + проверенный второй порядок для объединенного фактора.

### 3.1. Качество подгонки (лист `fit_measures`)
- **n** (`n`) = **774**
- **CFI** (`cfi`) = **0.995**
- **TLI** (`tli`) = **0.993**
- **RMSEA** (`rmsea`) = **0.074** с ДИ (`rmsea_ci_lower`, `rmsea_ci_upper`) = **0.069 / 0.079**
- **SRMR** (`srmr`) = **0.046**

### 3.2. Нагрузки второго порядка (лист `loadings_second_order`)
Столбцы те же, что в initial: `loading_std`, `loading_unstd`, `se`, `z`, `pvalue`.

| factor          | indicator   |   loading_std |   loading_unstd |    se |      z | pvalue   |
|:----------------|:------------|--------------:|----------------:|------:|-------:|:--------|
| perceived_value | benefit     |         0.962 |           3.528 | 0.400 |  8.820 | 0       |
| perceived_value | uniqueness  |         0.929 |           2.517 | 0.252 |  9.995 | 0       |
| perceived_value | assortment  |         0.928 |           2.498 | 0.236 | 10.591 | 0       |

То есть в финальной HCM второй порядок устойчиво подтвержден: `loading_std` = **0.928–0.962**, `pvalue` = **0**.

### 3.3. Нагрузки первого порядка (лист `loadings_first_order`)
Полная таблица (HCM, первый порядок):

| factor           | indicator                                  |   loading_std |   loading_unstd |    se |      z | pvalue   |
|:----------------|:--------------------------------------------|--------------:|----------------:|------:|-------:|:--------|
| assortment      | assortment_item_category_coverage_likert    |         0.827 |           0.232 | 0.040 |  5.794 | 6.88e-09 |
| assortment      | assortment_item_price_range_likert          |         0.746 |           0.209 | 0.036 |  5.789 | 7.10e-09 |
| assortment      | assortment_item_wide_choice_likert          |         0.693 |           0.194 | 0.033 |  5.809 | 6.28e-09 |
| benefit         | benefit_item_value_money_likert             |         0.842 |           0.225 | 0.039 |  5.799 | 6.69e-09 |
| benefit         | benefit_item_choose_same_price_likert       |         0.828 |           0.221 | 0.039 |  5.702 | 1.23e-08 |
| benefit         | benefit_item_saves_money_likert             |         0.697 |           0.186 | 0.032 |  5.751 | 8.85e-09 |
| uniqueness      | uniqueness_item_new_interest_likert         |         0.819 |           0.348 | 0.030 | 11.735 | 0       |
| uniqueness      | uniqueness_item_visit_for_pl_likert         |         0.798 |           0.339 | 0.027 | 12.414 | 0       |
| uniqueness      | uniqueness_item_unique_features_likert      |         0.714 |           0.303 | 0.026 | 11.597 | 0       |
| attitude        | attitude_item_prefer_available_likert       |         0.875 |           0.875 | 0.012 | 73.316 | 0       |
| attitude        | attitude_item_brand_alternative_likert      |         0.842 |           0.842 | 0.014 | 59.792 | 0       |
| retailer_img    | retailer_brand_item_logo_quality_trust_likert|        0.886 |           0.886 | 0.011 | 80.751 | 0       |
| retailer_img    | retailer_brand_item_quality_guarantee_likert |        0.878 |           0.878 | 0.012 | 75.921 | 0       |
| loyalty_retailer| loyalty_retailer_choose_store_likert        |         0.895 |           0.895 | 0.012 | 77.137 | 0       |
| loyalty_retailer| loyalty_retailer_intend_continue_likert     |         0.821 |           0.821 | 0.015 | 55.394 | 0       |
| loyalty_retailer| loyalty_retailer_like_store_likert          |         0.721 |           0.721 | 0.020 | 35.275 | 0       |
| loyalty_item    | loyalty_item_buy_more_likert                |         0.856 |           0.856 | 0.013 | 65.012 | 0       |
| loyalty_item    | loyalty_item_recommend_likert               |         0.823 |           0.823 | 0.015 | 55.362 | 0       |
| loyalty_item    | loyalty_item_prefer_likert                  |         0.821 |           0.821 | 0.015 | 54.017 | 0       |

### 3.4. Надежность факторов в финальной HCM (лист `reliability_alpha_cr`)
В финальной версии надежность держится на тех же столбцах:
- **альфа Кронбаха** (`cronbach_alpha`)
- **композитная надежность (CR)** (`composite_reliability`)

| factor           | cronbach_alpha |   composite_reliability |
|:-----------------|:---------------|------------------------:|
| assortment       | 0.710          |                   0.800 |
| attitude         | 0.794          |                   0.848 |
| benefit          | 0.764          |                   0.834 |
| loyalty_item     | 0.833          |                   0.873 |
| loyalty_retailer | 0.799          |                   0.855 |
| perceived_value  | NA             |                   0.958 |
| retailer_img     | 0.837          |                   0.875 |
| uniqueness       | 0.766          |                   0.821 |

Комментарий по `perceived_value`: альфа Кронбаха (`cronbach_alpha`) = **NA**, потому что это **фактор 2-го порядка**, а CR (`composite_reliability`) = **0.958** дает количественную опору, что общий “зонтик” измеряется устойчиво.

---

## Финальный вывод (логика решений по шагам)

1) В `cfa_initial.xlsx` модель в целом имеет сильную подгонку (например, `cfi` = **0.992**, `tli` = **0.990**, `srmr` = **0.050**), но по надежности и нагрузкам видны слабые места:  
   - `cannibalization`: `cronbach_alpha` = **0.445**, `composite_reliability` = **0.454**, и низкая нагрузка `loading_std` = **0.242** у `cannibalization_item_not_good_likert`.  
   - `stm`: `cronbach_alpha` = **0.536**, `composite_reliability` = **0.598**, и крайне низкая нагрузка `loading_std` = **0.153** у `stm_freq`.

2) Там же в initial три фактора (`assortment`, `benefit`, `uniqueness`) показывают очень высокие латентные корреляции (`corr` = **0.861–0.918** при `pvalue` = **0**), что дает основание тестировать общий фактор 2-го порядка.

3) В `cfa_second_order_loadings.xlsx` проверка “двухэтажности” подтверждается: второй порядок `perceived_value` имеет высокие нагрузки на три фактора (`loading_std` = **0.904–0.965**, `p_value` ≤ **2.74e-08**), а показатели подгонки остаются сильными (`cfi` = **0.995**, `srmr` = **0.040**).

4) В `cfa_hcm_loadings_reliability.xlsx` итоговая HCM фиксирует эту структуру как финальную: нагрузки второго порядка (`loading_std` = **0.928–0.962**, `pvalue` = **0**) и надежность ключевых факторов (например, `retailer_img`: `cronbach_alpha` = **0.837**, `composite_reliability` = **0.875**) дают прозрачную метриками-обоснованную опору для использования модели дальше в SEM/PLS-SEM.
