package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.List;


@Entity
@Setter
@Getter
@Table(name = "hbl")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class )
@SQLDelete(sql = "UPDATE hbl SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Hbl extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Type(type = "jsonb")
    @Column(name = "hbl_data", columnDefinition = "jsonb")
    private HblDataDto hblData;

    @Type(type = "jsonb")
    @Column(name = "container_data", columnDefinition = "jsonb")
    private List<HblContainerDto> hblContainer;

    @Type(type = "jsonb")
    @Column(name = "cargo_data", columnDefinition = "jsonb")
    private List<HblCargoDto> hblCargo;

    @Type(type = "jsonb")
    @Column(name = "notify_party_data", columnDefinition = "jsonb")
    private List<HblPartyDto> hblNotifyParty;

}
