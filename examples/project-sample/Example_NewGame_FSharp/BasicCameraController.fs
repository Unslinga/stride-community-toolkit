namespace Example_NewGame_FSharp

open System
open Stride.Core
open Stride.Core.Mathematics
open Stride.Engine
open Stride.Input

type Movement =
    { Translation: Vector3
      Rotation: Vector2 }

/// Removed touch navigation
type public BasicCameraController() =
    inherit SyncScript()

    let maximumPitch = MathUtil.PiOverTwo * 0.99f
    let upVector = Vector3.UnitY

    let movementSpeed = 5f
    let rotationSpeed = 3f
    let speedFactor = 5f

    member this.processInput() : Movement =
        let deltaTime = float32 this.Game.UpdateTime.Elapsed.TotalSeconds

        let keys =
            if this.Input.HasKeyboard then
                this.Input.DownKeys |> Seq.toList
            else
                []

        /// Utility method (might be good in an FSharp library)
        let getKey (key: Keys) (keys: Keys list) = keys |> List.exists (fun k -> k = key)

        /// Utility method (might be good in an FSharp library)
        let (|IsKeyDown|_|) (key: Keys) (keys: Keys list) =
            match getKey key keys with
            | true -> Some key
            | false -> None

        let getKey (key: Keys) = getKey key keys

        let translation =
            let x (m: Vector3) : Vector3 =
                let pos =
                    match keys with
                    | IsKeyDown Keys.Right _
                    | IsKeyDown Keys.D _ -> 1f
                    | _ -> 0f

                let neg =
                    match keys with
                    | IsKeyDown Keys.Left _
                    | IsKeyDown Keys.A _ -> 1f
                    | _ -> 0f

                Vector3(m.X + pos - neg, m.Y, m.Z)

            let y (m: Vector3) : Vector3 =
                let pos =
                    match keys with
                    | IsKeyDown Keys.Space _ -> 1f
                    | _ -> 0f

                let neg =
                    match keys with
                    | IsKeyDown Keys.LeftCtrl _ -> 1f
                    | _ -> 0f

                Vector3(m.X, m.Y + pos - neg, m.Z)

            let z (m: Vector3) : Vector3 =
                let pos =
                    match keys with
                    | IsKeyDown Keys.Up _
                    | IsKeyDown Keys.W _ -> 1f
                    | _ -> 0f

                let neg =
                    match keys with
                    | IsKeyDown Keys.Down _
                    | IsKeyDown Keys.S _ -> 1f
                    | _ -> 0f

                Vector3(m.X, m.Y, (m.Z + pos - neg) * -1f)

            let normalize (m: Vector3) : Vector3 =
                if m.Length() > 1f then Vector3.Normalize m else m

            let speed =
                let baseSpeed = 1f * deltaTime * movementSpeed

                match keys with
                | IsKeyDown Keys.LeftShift _
                | IsKeyDown Keys.RightShift _ -> baseSpeed * speedFactor
                | _ -> baseSpeed

            Vector3.Multiply(Vector3.Zero |> x |> y |> z |> normalize, speed)

        let rotation =
            let pitch (r: Vector2) : Vector2 =
                let pos = if getKey Keys.I then 1f else 0f
                let neg = if getKey Keys.K then 1f else 0f
                Vector2(r.X + pos - neg, r.Y)

            let yaw (r: Vector2) : Vector2 =
                let pos = if getKey Keys.J then 1f else 0f
                let neg = if getKey Keys.L then 1f else 0f
                Vector2(r.X, r.Y + pos - neg)

            let normalize (r: Vector2) : Vector2 =
                if r.Length() > 1f then Vector2.Normalize(r) else r

            let speed = 1f * deltaTime * rotationSpeed

            Vector2.Multiply(Vector2.Zero |> pitch |> yaw |> normalize, speed)

        { Translation = translation
          Rotation = rotation }

    member this.UpdateTransform(movement: Movement) =
        let rotation = this.Entity.Transform.Rotation
        let rotationMatrix = (Matrix.RotationQuaternion(rotation))

        let position = this.Entity.Transform.Position
        let translation = Vector3.TransformCoordinate(movement.Translation, rotationMatrix)
        this.Entity.Transform.Position <- Vector3.Add(position, translation)

        let forward = rotationMatrix.Forward
        let right = Vector3.Cross(&forward, &upVector) |> Vector3.Normalize

        let currentPitch = MathUtil.PiOverTwo - MathF.Acos(Vector3.Dot(forward, upVector))

        let pitch =
            (right,
             MathUtil.Clamp(currentPitch + movement.Rotation.X, -maximumPitch, maximumPitch)
             - currentPitch)
            |> Quaternion.RotationAxis

        let yaw = (upVector, movement.Rotation.Y) |> Quaternion.RotationAxis

        let combined = Quaternion.Multiply(&pitch, &yaw)

        this.Entity.Transform.Rotation <- Quaternion.Multiply(&rotation, &combined)

    override this.Update() : unit =
        this.processInput () |> this.UpdateTransform
