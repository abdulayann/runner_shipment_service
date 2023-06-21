package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.*;
import lombok.Data;
import lombok.ToString;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;

@Data
public class ShipmentDetailsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private List<PartiesDetails> parties;
    private BlDetails blDetails;
    private CarrierDetails carrierDetails;
    private MeasurementDetails measurementDetails;
    private String houseBill;
    private String transportMode;
    private String direction;
    private String shipmentType;
    private Integer status;
    private String source;
    private String jobType;
    private String serviceType;
    private String masterBill;
    private String bookingReference;
    private String consolRef;
    private Long salesAgent;
    private String paymentTerms;
    private String incoterms;
    private String shipmentId;
    private Boolean isDomestic;
    private Integer assignedTo;
    private String additionalTerms;
    private String goodsDescription;
    private PickupDetails pickupDetails;
    private DeliveryDetails deliveryDetails;
}
