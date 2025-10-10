package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import lombok.*;

import jakarta.persistence.Table;
import org.hibernate.annotations.JdbcTypeCode;
import jakarta.persistence.Column;
import jakarta.persistence.Id;
import org.hibernate.type.SqlTypes;


@Table(name = "container_package_payload")
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
public class ContainerPackageDiffDto {
    @Id
    @Column(name = "si_id")
    private Long siId;   // <-- must be set manually, no auto-generation

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "payload_json", columnDefinition = "jsonb")
    private ContainerPackageSiPayload payloadJson;
}
