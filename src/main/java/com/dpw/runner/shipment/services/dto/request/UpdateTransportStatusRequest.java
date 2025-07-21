package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
@Data
@Builder
@ApiModel("Routings update transport status Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class UpdateTransportStatusRequest implements Serializable {
    @NotNull(message = "EntityId can not be null")
    private Long entityId;
    @NotBlank(message = "Entity Type can not be blank")
    private String entityType;
    @NotNull(message = "Transport info can not be null")
    private TransportInfoStatus transportInfoStatus;
}
