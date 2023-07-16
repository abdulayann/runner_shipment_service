package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;


@Entity
@Setter
@Getter
@Table(name = "views")
@Accessors(chain = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class )
@SQLDelete(sql = "UPDATE views SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Views extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Type(type = "jsonb")
    @Column(name = "columns", columnDefinition = "jsonb")
    private Map<String, String> columns;

    @Type(type = "jsonb")
    @Column(name = "criteria", columnDefinition = "jsonb")
    private List<FilterCriteria> criteria;

    @Column(name = "is_public")
    private Boolean isPublic;

    @Column(name = "entity")
    private String entity;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;
}
