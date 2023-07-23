package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.Map;


@Entity
@Setter
@Getter
@Table(name = "hbl")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class )
public class Hbl extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Type(type = "jsonb")
    @Column(name = "hbl_data", columnDefinition = "jsonb")
    private Map<String, Object> hblData;

    @Type(type = "jsonb")
    @Column(name = "container_data", columnDefinition = "jsonb")
    private Map<String, Object> hblContainer;

    @Type(type = "jsonb")
    @Column(name = "cargo_data", columnDefinition = "jsonb")
    private Map<String, Object> hblCargo;

    @Type(type = "jsonb")
    @Column(name = "notify_party_data", columnDefinition = "jsonb")
    private Map<String, Object> hblNotifyParty;

}
