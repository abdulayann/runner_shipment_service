package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("Enum Constant Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class EnumConstantResponse implements IRunnerResponse {
    private String name;
    private int id;
    private String description;
}
