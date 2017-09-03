-- code from blog about free monads http://degoes.net/articles/modern-fp

-- type Natural f g = forall a . f a -> g a

type Path = String
type Bytes = [] Char

data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path ([] Path -> a)

type CloudFilesAPI a = Free CloudFilesF a

saveFile :: Path -> Bytes -> CloudFilesAPI ()
saveFile path bytes = liftF (SaveFile path bytes ())

listFiles :: Path -> CloudFilesAPI ([] Path)
listFiles path = liftF (ListFiles path id)

data HttpF a
  = GET    Path (Bytes -> a)
  | PUT    Path Bytes (Bytes -> a)
  | POST   Path Bytes (Bytes -> a)
  | DELETE Path (Bytes -> a)


-- cloudFilesI :: forall a . CloudFilesF a -> Free HttpF a


data LogF a = Log Level String a

data Level = ERROR | WARNING | DEBUG | TRACE | NONE


-- logCloudFilesI :: forall a. CloudFilesF a -> Free LogF Unit
logCloudFilesI (SaveFile p _ _) = liftF $ Log Debug ("Saving file to " ++ show p) Unit
logCloudFilesI (ListFiles p _)  = liftF $ Log Debug ("Listing files at " ++ show p) Unit


-- loggingCloudFilesI :: forall a. CloudFilesF a -> Free (Coproduct LogF HttpF) a
loggingCloudFilesI op = toLeft (logCloudFilesI op) *> toRight (cloudFilesI op)

-- executor :: forall a. Coproduct LogF HttpF a -> IO a