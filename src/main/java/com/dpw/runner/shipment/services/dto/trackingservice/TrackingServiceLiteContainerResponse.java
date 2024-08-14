package com.dpw.runner.shipment.services.dto.trackingservice;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
