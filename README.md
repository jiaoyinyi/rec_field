rec_field
=====

解析record结构字段定义，可用于record和其他数据之间的转换。  

* 该库只提供record结构解析，解析后信息输出到rec_field_data.erl文件，具体转换逻辑需要自己实现。

### 使用例子

#### 1、给字段定义类型

```
-record(role, {
    rid                     :: non_neg_integer()
    , srv_id                :: binary()
    , name = <<>>           :: binary()
    , lev = 1               :: pos_integer()
    , partners = []         :: [{record, partner}]
    , m_package             :: {record, m_package}
}).

-record(partner, {
    id                       :: pos_integer()
    , bid                    :: pos_integer()
}).

-record(package, {
    type                     :: pos_integer()
    , items = []             :: [{record, item}]
    , capacity               :: pos_integer()
    , size                   :: non_neg_integer()
}).

-record(m_package, {
    packages                 :: #{pos_integer() => #package{}}
}).

-record(item, {
    id                       :: pos_integer()
    , bid                    :: pos_integer()
}).
```

#### 2、配置gen_rec.sh参数

要生成的头文件路径（`GEN_REC_INC_PATH`）、生成目标文件路径（`GEN_REC_OUT_PATH`）

#### 3、生成rec_term_data.erl文件

```./gen_rec.sh gen```

### 支持字段类型

| 类型                    | 定义                                                              | 默认值              |
|-----------------------|-----------------------------------------------------------------|------------------|
| 原子                    | atom()                                                          | undefined        |
| 整形                    | integer(), pos_integer(), neg_integer(), non_neg_integer()      | 按类型确定，通常为0       |
| 浮点形                   | float()                                                         | 0.0              |
| Bool                  | boolean()                                                       | false            |
| 二进制                   | binary()                                                        | <<>>             | 
| 记录                    | #record{}, {record,RecName}                                     | 按record子字段生成默认值  |
| 基础类型列表                | [base_type()]                                                   | []               |
| 记录类型列表                | [#record{}], [{record,RecName}]                                 | []               |
| Key为基础类型，Val为基础类型的Map | #{base_type() => base_type()}                                   | #{}              |
| Key为基础类型，Val为记录类型的Map | #{base_type() => #record{}}, #{base_type() => {record,RecName}} | #{}              |
| 基础类型元组                | {base_type(),base_type(),...}                                   | 按tuple子字段类型生成默认值 |
| 基础类型范围类型              | base_type() \| base_type()      | 第一个定义值为默认值       |
| 其他类型(兼容处理)            | term()                                                          | undefined        |

#### 基础字段类型 base_type()

atom(), integer(), pos_integer(), neg_integer(), non_neg_integer(), float(), boolean(), binary()

### 其他

1、剔除不生成的头文件 在`gen_rec.hrl`的`EXCLUDE_HRLS`宏配置  
2、剔除不生成的记录 在`gen_rec.hrl`的`EXCLUDE_RECORDS`宏配置  
3、未支持的字段类型会自动转成term()类型兼容处理，默认值为undefined，且该类型在反序列化不会自动版本转换