package com.dpw.runner.shipment.services.entity;


import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.util.UUID;


@Entity
@Setter
@Getter
@Table(name = "default_views")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@SQLDelete(sql = "UPDATE default_views SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class DefaultViews {

    private static final long serialVersionUID = 190794279984274725L;

    @Id
    @ToString.Include
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "guid")
    private UUID guid;

    @Column(name = "default_view_id")
    private Long defaultViewId;

    @Column(name = "username")
    private String username;

    @Column(name = "entity")
    private String entity;
}
