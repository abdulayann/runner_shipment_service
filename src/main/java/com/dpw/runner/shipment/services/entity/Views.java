package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import jakarta.persistence.*;
import org.hibernate.type.SqlTypes;

import java.util.Map;


@Entity
@Setter
@Getter
@Table(name = "views")
@Accessors(chain = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE views SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
@SuppressWarnings("java:S1948")
public class Views extends BaseEntity {

    private static final long serialVersionUID = 190794279984274725L;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "columns", columnDefinition = "jsonb")
    private Map<String, Object> columns;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "criteria", columnDefinition = "jsonb")
    private Map<String, Object> criteria;

    @Column(name = "is_default")
    private Boolean isDefault;

    @Column(name = "name")
    private String name;

    @Column(name = "entity")
    private String entity;
}
