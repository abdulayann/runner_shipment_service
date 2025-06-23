package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.utils.TenantIdData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Embeddable
@SuppressWarnings("java:S1700") // A field should not duplicate the name of its containing class
public class TriangulationPartner implements Serializable {

    @Column(name = "partner_id")
    @TenantIdData
    private Long triangulationPartner;

    @Column(name = "is_accepted")
    private Boolean isAccepted;
}
