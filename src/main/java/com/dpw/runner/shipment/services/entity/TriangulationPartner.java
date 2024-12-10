package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.utils.TenantIdData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Embeddable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Embeddable
public class TriangulationPartner {

    @Column(name = "partner_id")
    @TenantIdData
    private Long triangulationPartner;

    @Column(name = "is_accepted")
    private Boolean isAccepted;
}
