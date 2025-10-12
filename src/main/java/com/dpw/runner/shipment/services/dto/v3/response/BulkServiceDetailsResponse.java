package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.List;

@Data
@Builder
@Schema(description = "Bulk Service Details Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkServiceDetailsResponse {
    private String message;
    private List<ServiceDetailsResponse> serviceDetailsResponseList;
}
