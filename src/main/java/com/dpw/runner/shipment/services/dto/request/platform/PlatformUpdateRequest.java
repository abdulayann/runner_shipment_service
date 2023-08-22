package com.dpw.runner.shipment.services.dto.request.platform;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@Data
public class PlatformUpdateRequest extends CommonRequest implements IRunnerRequest {

    private String booking_reference_code;
    private List<String> cancel_load;
    private List<LoadRequest> add_load;
    private List<AlterLoadRequest> alter_load;
    private RouteRequest route;
    private List<ChargeRequest> charges;
    private List<DocumentMetaDTO> document_meta;
    private String status;
    private String pickup_date;
    private String eta;
    private String ets;
    private MotherVesselDetailsRequest mother_vessel_details;
    private InvoiceDetailsDTO invoice_details;
    private ESIDetailsDTO eSI_details;
    private String barge_voyage_number;
    private String vessel_name;
    private String voyage;
    private VehicleDetailsDTO vehicle_details;

}
