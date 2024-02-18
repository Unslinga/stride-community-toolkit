﻿using Stride.Engine;
using Stride.Physics;
using Stride.Rendering;

namespace Stride.CommunityToolkit.Engine;

public class Primitive2DCreationOptions
{
    /// <summary>
    /// Gets or sets the name of the entity.
    /// </summary>
    public string? EntityName { get; set; }

    /// <summary>
    /// Gets or sets the material to be applied to the primitive model.
    /// </summary>
    public Material? Material { get; set; }

    /// <summary>
    /// Determines whether to include a collider component in the entity. Defaults to true.
    /// </summary>
    public bool IncludeCollider { get; set; } = true;

    /// <summary>
    /// Gets or sets the size of the primitive model. If null, default dimensions are used.
    /// </summary>
    public Vector2? Size { get; set; }

    /// <summary>
    /// Gets or sets the render group for the entity. Defaults to RenderGroup.Group0.
    /// </summary>
    public RenderGroup RenderGroup { get; set; } = RenderGroup.Group0;

    /// <summary>
    /// Gets or sets the physics component to be added to the entity. Defaults to a new instance of RigidbodyComponent.
    /// </summary>
    public PhysicsComponent? PhysicsComponent { get; set; } = new RigidbodyComponent();

    public float Depth { get; set; } = 0.04f;
}