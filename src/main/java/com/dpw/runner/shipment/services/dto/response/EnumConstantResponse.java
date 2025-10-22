package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.List;

@Data
@Builder
@Schema(description = "Enum Constant Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class EnumConstantResponse implements IRunnerResponse {
    private String name;
    private int id;
    private String description;
    private List<String> meta;
}
