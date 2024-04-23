open Caqti_platform
open Caqti_platform_unix

module Loader = Driver_loader.Make (Caqti_miou.System) (System_unix)

include Connector.Make (Caqti_miou.System) (Caqti_miou.Pool) (Loader)
