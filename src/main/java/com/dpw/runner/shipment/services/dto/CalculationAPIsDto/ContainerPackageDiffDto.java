package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;

import jakarta.persistence.Table;

import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import jakarta.persistence.Column;
import jakarta.persistence.Id;


@Table(name = "container_package_payload")
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
public class ContainerPackageDiffDto {
    @Id
    @Column(name = "si_id")
    private Long siId;   // <-- must be set manually, no auto-generation

    @Type(type = "jsonb")
    @Column(name = "payload_json", columnDefinition = "jsonb")
    private ContainerPackageSiPayload payloadJson;
}
