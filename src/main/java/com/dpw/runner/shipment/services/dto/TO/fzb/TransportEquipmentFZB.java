package com.dpw.runner.shipment.services.dto.TO.fzb;

import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TransportEquipmentFZB {

    @NotBlank(message = "ID cannot be blank")
    @Size(max = 70, message = "ID length must be less than or equal to 70 characters")
    private String ID;

    @NotBlank(message = "Characteristic code cannot be blank")
    @Size(max = 35, message = "Characteristic code length must be less than or equal to 35 characters")
    private String characteristicCode;

    @NotBlank(message = "Characteristic cannot be blank")
    @Size(max = 35, message = "Characteristic length must be less than or equal to 35 characters")
    private String characteristic;

    @NotBlank(message = "Seal number cannot be blank")
    @Size(max = 35, message = "Seal number length must be less than or equal to 35 characters")
    private String affixedLogisticsSealId;
}
