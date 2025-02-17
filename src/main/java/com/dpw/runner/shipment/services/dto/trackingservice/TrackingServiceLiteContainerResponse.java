package com.dpw.runner.shipment.services.dto.trackingservice;

import lombok.*;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TrackingServiceLiteContainerResponse {

    private List<LiteContainer> containers;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    public static class LiteContainer extends ContainerBase {

        private String containerNumber;
        private String identifierType;
        private String identifierValue;
    }

}
