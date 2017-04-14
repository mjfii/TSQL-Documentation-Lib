#' Return markdown for database
#'
#' This function returns a data frame of ordered markdown that can be used to knit a document.
#' @param connection ODBC Connection Class
#' @keywords tsql
#' @export
#' @examples
#' cnn <- get.connection('<server_name>','<database_name>')
#' md <- get.markdown(cnn)
#' View(md)
get.markdown <- function(connection) {
  
  query <- "

declare @crlf nvarchar(2)=concat(N'  ',char(13),char(10),N'  ');
 
  with [tables] as
  (
  select
  concat(N'/3/',row_number() over(order by a.[name]),N'/') [objectPointer], --a.[object_id]
  a.[object_id] [objectID],
  object_schema_name(a.[object_id]) [schemaName],
  a.[name] [tableName],
  concat(N'### [',object_schema_name(a.[object_id]),'].[',a.[name], N']',@crlf) [documentationValue]
  from
  sys.tables a
  where
  a.[is_ms_shipped]=0
  ), [table_headers] as
  (
  select
  concat([objectPointer],N'1.1/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'#### Columns:',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'2.1/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'#### Keys:',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'6.1/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'#### Statistics:',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'7.1/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'#### Properties:',@crlf) [documentationValue]
  from
  [tables]
  
  -- before
  union all
  select
  concat([objectPointer],N'1/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'2/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'6/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'7/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  
  -- after      
  union all
  select
  concat([objectPointer],N'1.1/0/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'2.1/0/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'6.1/0/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  union all
  select
  concat([objectPointer],N'7.1/0/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables]
  
  union all
  select
  concat([objectPointer],N'8/0/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables] 
  
  -- table end
  union all
  select
  concat([objectPointer],N'8/1/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'---',@crlf) [documentationValue]
  from
  [tables] 
  
  union all
  select
  concat([objectPointer],N'8/2/') [objectPointer],
  [objectID],
  [schemaName],
  [tableName],
  concat(N'',@crlf) [documentationValue]
  from
  [tables] 
  ), [table_description] as
  (
  select
  concat([objectPointer],N'0/') [objectPointer],
  concat(N'*',isnull(convert(nvarchar(max),a.[value]),quotename(x.[schemaName])+N'.'+quotename(x.[tableName])+N' description and/or summary missing.'),N'*') [documentationValue]
  from
  [tables] x
  left join
  sys.extended_properties a on x.[objectID]=a.[major_id] and a.[class]=1 and a.[minor_id]=0 and a.[name]='Description'
  )
  , [columns] as
  (
  select
  concat([objectPointer],N'1.1/',a.[column_id],N'/') [objectPointer],
  a.[object_id] [objectID],
  object_schema_name(a.[object_id]) [schemaName],
  object_name(a.[object_id]) [tableName],
  concat(N'    ',quotename(a.[name]),N' (',
  case b.[name]
  when N'char' then b.[name]+N'('+convert(nvarchar(10),a.[max_length])+N')'
  when N'nchar' then b.[name]+N'('+convert(nvarchar(10),a.[max_length]/2)+N')'
  when N'varchar' then b.[name]+N'('+ isnull(nullif(convert(nvarchar(10),a.[max_length]),N'-1'),N'max') +N')'
  when N'nvarchar' then b.[name]+N'('+ isnull(nullif(convert(nvarchar(10),a.[max_length]/2),N'-1'),N'max') +N')'
  when N'binary' then b.[name]+N'('+isnull(nullif(convert(nvarchar(10),a.[max_length]),N'-1'),N'max')+N')'
  when N'varbinary' then b.[name]+N'('+isnull(nullif(convert(nvarchar(10),a.[max_length]),N'-1'),N'max')+N')'
  when N'datetime2' then b.[name]+N'('+convert(nvarchar(10),a.[scale])+N')'
  when N'decimal' then b.[name]+N'('+convert(nvarchar(10),a.[precision])+N','+convert(nvarchar(10),a.[scale])+N')'
  when N'numeric' then b.[name]+N'('+convert(nvarchar(10),a.[precision])+N','+convert(nvarchar(10),a.[scale])+N')'
  else b.[name]
  end,
  case
  when c.[object_id] is not null then concat(N' identity(',convert(int,c.[seed_value]),N',',convert(int,c.[increment_value]),N')')
  else N''
  end,
  case
  when a.[is_sparse]=1 then N', sparse'
  when a.[is_nullable]=0 then N', not null'
  else ', null'
  end,
  case
  when a.[collation_name]=convert(sysname,databasepropertyex(db_name(db_id()),N'collation')) then N')'
  when a.[collation_name] is not null then concat(N', collated with ',a.[collation_name],N')')
  else N')'
  end,N'  '
  ) [documentationValue]
  from
  [tables] x
  inner join
  sys.columns a on x.[objectID]=a.[object_id]
  left join
  sys.types b on a.[user_type_id]=b.[user_type_id] 
  left join
  sys.identity_columns c on a.[object_id]=c.[object_id]
  and a.[column_id]=c.[column_id]
  ), [keys] as
  (
  select
  concat([objectPointer],N'2.1/',case a.[type] when N'UQ' then 2 else 1 end,N'/') [objectPointer],
  x.[objectID] [objectID],
  object_schema_name(x.[objectID]) [schemaName],
  object_name(x.[objectID]) [tableName],
  concat(N'    ',quotename(a.[name]), N' (',case when b.[type]=1 then N'clustered ' else N'' end,case a.[type] when N'UQ' then N'alternate key' else N'primary key' end,N')') [documentationValue]
  from
  [tables] x
  inner join
  sys.key_constraints a on x.[objectID]=a.[parent_object_id]
  inner join
  sys.indexes b on a.[parent_object_id]=b.[object_id]
  and a.[unique_index_id]=b.[index_id]
  union all
  select
  concat([objectPointer],N'2.1/',3,N'/') [objectPointer],
  a.[parent_object_id] [objectID],
  object_schema_name(a.[parent_object_id]) [objectSchema],
  object_name(a.[parent_object_id]) [objectName],
  concat(N'    ',quotename(a.[name]),char(13),char(10),N'       (foreign key references ',quotename(object_schema_name(a.[referenced_object_id])),N'.',quotename(object_name(a.[referenced_object_id])),N' on {',stuff(b.[cols],1,1,N''),N'})') [documentationValue]
  from
  [tables] x
  inner join
  sys.foreign_keys a on x.[objectID]=a.[parent_object_id]
  cross apply
  (
  select
  concat(N',',quotename(c.[name]),N'=',quotename(d.[name]))
  from
  sys.foreign_key_columns b
  inner join
  sys.columns c on b.[parent_object_id]=c.[object_id]
  and b.[parent_column_id]=c.[column_id]        
  inner join
  sys.columns d on b.[referenced_object_id]=d.[object_id]
  and b.[referenced_column_id]=d.[column_id]  
  where
  a.[object_id]=b.[constraint_object_id]
  for xml path(N'')
  ) b(cols)
  ), [statistics] as
  (
  select
  concat(x.[objectPointer],N'6.1/',a.[stats_id],N'/') [objectPointer],
  [object_id] [objectID],
  object_schema_name(a.[object_id]) [schemaName],
  object_name(a.[object_id]) [tableName],
  concat(N'    ',quotename(a.[name]),N' (',stuff(b.[cols],1,1,N''),N')') [documentationValue]
  from
  [tables] x
  inner join
  sys.stats a on x.[objectID]=a.[object_id] and a.[user_created]=1
  cross apply
  (
  select
  concat(N',',quotename(c.[name]))
  from
  sys.stats_columns b
  inner join
  sys.columns c on b.[object_id]=c.[object_id]
  and b.[column_id]=c.[column_id]
  inner join
  sys.stats d on b.[object_id]=d.[object_id]
  and b.[stats_id]=d.[stats_id]
  where
  a.[object_id]=b.[object_id]
  and
  a.[stats_id]=b.[stats_id]
  and
  d.[user_created]=1
  for xml path(N'')
  ) b(cols)
  ), [properties] as
  (
  select
  concat(x.[objectPointer],N'7.1/1/') [objectPointer],
  [object_id] [objectID],
  object_schema_name(a.[object_id]) [schemaName],
  object_name(a.[object_id]) [tableName],
  concat(N'    [Create Date]: ',[create_date]) [documentationValue]
  from
  [tables] x
  inner join
  sys.tables a on x.[objectID]=a.[object_id]
  where
  a.[is_ms_shipped]=0
  union all
  select
  concat(x.[objectPointer],N'7.1/2/') [objectPointer],
  [object_id] [objectID],
  object_schema_name(a.[object_id]) [schemaName],
  object_name(a.[object_id]) [tableName],
  concat(N'    [Modify Date]: ',[modify_date]) [documentationValue]
  from
  [tables] x
  inner join
  sys.tables a on x.[objectID]=a.[object_id]
  where
  a.[is_ms_shipped]=0
  
  union all
  
  select
  concat(x.[objectPointer],N'7.1/',row_number() over(order by a.[name])+2,'/') [objectPointer],
  x.[objectID] [objectID],
  object_schema_name(x.[objectID]) [schemaName],
  object_name(x.[objectID]) [tableName],
  concat(N'    [',a.[name],']: ',convert(nvarchar(4000),a.[value])) [documentationValue]
  from
  [tables] x
  inner join
  sys.extended_properties a on x.[objectID]=a.[major_id]
  where
  a.[minor_id]=0
  and
  a.[name]!=N'Description'
  
  ), [document] as
  (
  select N'/0/' [objectPointer],concat(N'# ',quotename(db_name(db_id())),N' Data Definition Documentation') [documentationValue]
  union all
  select N'/0/0.1/' [objectPointer], N'' [documentationValue]
  union all
  select N'/0/1/' [objectPointer],N'---' [documentationValue]
  union all
  select N'/0/1/0/' [objectPointer], N'' [documentationValue]
  union all
  select N'/0/2/' [objectPointer],concat(N'*',convert(nvarchar(max),[value]) ,N'*') [documentationValue]
  from sys.extended_properties
  where [class]=0 and [name]=N'Description'
  union all
  select N'/0/2/0/' [objectPointer], N'' [documentationValue]
  union all
  select N'/1/' [objectPointer],N'## *Database Settings:*' [documentationValue]
  union all
  select N'/1/1/' [objectPointer],concat(N'     [Collation]: ',convert(sysname,databasepropertyex(db_name(db_id()),N'collation'))) [documentationValue]
  union all
  select N'/1/2/' [objectPointer],concat(N'     [Compatibility Level]: ',(select convert(nvarchar(10),[compatibility_level]) from sys.databases where [name] = 'edw_oda')) [documentationValue]
  union all
  select N'/1/3/' [objectPointer],concat(N'     [Auto Create Statistics]: ',case convert(bit,databasepropertyex(db_name(db_id()),N'IsAutoCreateStatistics')) when 1 then N'Yes' else N'No' end) [documentationValue]
  union all
  select N'/1/4/' [objectPointer],concat(N'     [Auto Update Statistics]: ',case convert(bit,databasepropertyex(db_name(db_id()),N'IsAutoUpdateStatistics')) when 1 then N'Yes' else N'No' end) [documentationValue]
  
  
  union all
  select N'/1/98/' [objectPointer],N'' [documentationValue]
  union all
  select N'/1/99/' [objectPointer],N'---' [documentationValue]
  union all
  select N'/1/100/' [objectPointer],N'' [documentationValue]
  --union all
  --select N'/2/' [objectPointer],N'## *Schemas:*' [documentationValue]
  union all
  select N'/3/' [objectPointer],N'## *Tables:*' [documentationValue]
  union all
  select N'/3/0/' [objectPointer],N'' [documentationValue]
  union all
  select
  [objectPointer],[documentationValue]
  from
  [tables]
  union all
  select
  [objectPointer],[documentationValue]
  from
  [table_headers]
  union all
  select
  [objectPointer],[documentationValue]
  from
  [table_description]
  union all 
  select
  [objectPointer],[documentationValue]
  from
  [columns] a
  union all
  select
  [objectPointer],[documentationValue]
  from
  [keys] a
  union all
  select
  [objectPointer],[documentationValue]
  from
  [statistics] a
  union all
  select
  [objectPointer],[documentationValue]
  from
  [properties] a
  )
  select [documentationValue]
  from [document]
  order by convert(hierarchyid,[objectPointer]);


  "
  
  markdown <- RODBC::sqlQuery(connection, query, stringsAsFactors = FALSE)
  
  return(markdown)  
}
