package com.dpw.runner.shipment.services.dto.request;


import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Data
public class AutoCalculatePackingRequest extends PackingRequest {
    private String transportMode;
    private String containerCategory;
}
