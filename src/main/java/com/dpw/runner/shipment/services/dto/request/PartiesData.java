package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.entity.enums.PartyType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PartiesData {

    @NotNull(message = "party type is mandatory")
    private PartyType type;

    @NotNull(message = "address_id is mandatory")
    private Long addressId;

    @NotNull(message = "org_id is mandatory")
    private Long orgId;
}
