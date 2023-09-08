package com.dpw.runner.shipment.services.dto.request.platform;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;
import com.dpw.runner.shipment.services.dto.request.platform.AirCarrierDetailsRequest;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@Data
public class PlatformUpdateRequest extends CommonRequest implements IRunnerRequest {

    private String booking_reference_code;
    private String origin_code;
    private String destination_code;
    private String pol;
    private String pod;
    private String customer_email;
    private List<LoadRequest> load;
    private List<ChargesRequest> charges;
    private List<DocumentMetaDTO> document_meta;
    private String carrier_code;
    private String carrier_display_name;
    private AirCarrierDetailsRequest air_carrier_details;
    private String status;
    private String pickup_date;
    private LocalDateTime eta;
    private LocalDateTime ets;
    private MotherVesselDetailsRequest mother_vessel_details;
    private InvoiceDetailsDTO invoice_details;
    private ESIDetailsDTO eSI_details;
    private String barge_voyage_number;
    private String vessel_name;
    private String voyage;
    private VehicleDetailsDTO vehicle_details;

}
