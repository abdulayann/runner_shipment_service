package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;


@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RunnerResponse {
    List<ShipmentDetails> data;
    long pageNo;
    long count;
}
