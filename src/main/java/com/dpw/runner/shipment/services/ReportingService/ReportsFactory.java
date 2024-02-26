package com.dpw.runner.shipment.services.ReportingService;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Reports.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReportsFactory {
    @Autowired
    private ArrivalNoticeReport arrivalNoticeReport;
    @Autowired
    private ShippingInstructionReport shippingInstructionReport;
    @Autowired
    private BookingConfirmationReport bookingConfirmationReport;
    @Autowired
    private DeliveryOrderReport deliveryOrderReport;
    @Autowired
    private HblReport hblReport;
    @Autowired
    private HawbReport hawbReport;
    @Autowired
    private MawbReport mawbReport;
    @Autowired
    private CargoManifestReport cargoManifestReport;
    @Autowired
    private CommercialInvoiceReport commercialInvoiceReport;
    @Autowired
    private ConsolidatedPackingListReport consolidatedPackingListReport;
    @Autowired
    private FreightCertificationReport freightCertificationReport;
    @Autowired
    private ManifestConsolReport manifestConsolReport;
    @Autowired
    private ManifestShipmentReport manifestShipmentReport;
    @Autowired
    private AWBLabelReport awbLabelReport;
    @Autowired
    private ShippingRequestAirReport shippingRequestAirReport;
    @Autowired
    private PickupOrderReport pickupOrderReport;
    @Autowired
    private ProofOfDeliveryReport proofOfDeliveryReport;
    @Autowired
    private PackingListReport packingListReport;
    @Autowired
    private ShipmentCANReport shipmentCANReport;
    @Autowired
    private CustomsInstructionsReport customsInstructionsReport;
    @Autowired
    private PreAlertReport preAlertReport;

    @Autowired
    private ManifestPrintReport manifestPrintReport;

    @Autowired
    private ShippingRequestOutReport shippingRequestOutReport;

    @Autowired
    private ContainerManifestPrint containerManifestPrint;

    @Autowired
    private SeawayBillReport seawayBillReport;

    @Autowired
    private ShipTruckDriverProof shipTruckDriverProof;

    @Autowired
    private ShipTruckwayBillReport shipTruckwayBillReport;

    @Autowired
    private ConsTruckDriverProof consTruckDriverProof;

    @Autowired
    private ConsTruckwayBillReport consTruckwayBillReport;

    public IReport getReport(String key) {
        switch (key) {
            case ReportConstants.ARRIVAL_NOTICE:
                return arrivalNoticeReport;
            case ReportConstants.BOOKING_CONFIRMATION:
                return bookingConfirmationReport;
            case ReportConstants.DELIVERY_ORDER:
                return deliveryOrderReport;
            case ReportConstants.HOUSE_BILL:
                return hblReport;
            case ReportConstants.HAWB:
                return hawbReport;
            case ReportConstants.MAWB:
                return mawbReport;
            case ReportConstants.CARGO_MANIFEST:
                return cargoManifestReport;
            case ReportConstants.COMMERCIAL_INVOICE:
                return commercialInvoiceReport;
            case ReportConstants.CONSOLIDATED_PACKING_LIST:
                return consolidatedPackingListReport;
            case ReportConstants.FREIGHT_CERTIFICATION:
                return freightCertificationReport;
            case ReportConstants.EXPORT_CONSOL_MANIFEST:
                return manifestConsolReport;
            case ReportConstants.IMPORT_SHIPMENT_MANIFEST, ReportConstants.EXPORT_SHIPMENT_MANIFEST, ReportConstants.GENERATE_ISF_FILE:
                return manifestShipmentReport;
            case ReportConstants.AWB_LABEL:
                return awbLabelReport;
            case ReportConstants.SHIPPING_REQUEST_AIR:
                return shippingRequestAirReport;
            case ReportConstants.PICKUP_ORDER:
                return pickupOrderReport;
            case ReportConstants.PROOF_OF_DELIVERY:
                return proofOfDeliveryReport;
            case ReportConstants.PACKING_LIST:
                return packingListReport;
            case ReportConstants.SHIPMENT_CAN_DOCUMENT:
                return shipmentCANReport;
            case ReportConstants.CUSTOMS_INSTRUCTIONS:
                return customsInstructionsReport;
            case ReportConstants.PRE_ALERT:
                return preAlertReport;
            case ReportConstants.SHIPPING_REQUEST:
                return shippingRequestOutReport;
            case ReportConstants.MANIFEST_PRINT:
                return manifestPrintReport;
            case ReportConstants.CONTAINER_MANIFEST_PRINT:
                return containerManifestPrint;
            case ReportConstants.SHIPPING_INSTRUCTION:
                return shippingInstructionReport;
            case ReportConstants.SEAWAY_BILL:
                return seawayBillReport;
            case ReportConstants.IMPORT_CONSOL_MANIFEST:
                return manifestConsolReport;
            case ReportConstants.SHIP_TRUCKWAY_BILL:
                return shipTruckwayBillReport;
            case ReportConstants.CONS_TRUCKWAY_BIll:
                return consTruckwayBillReport;
            case ReportConstants.SHIP_TRUCK_DRIVER_PROOF:
                return shipTruckDriverProof;
            case ReportConstants.CONS_TRUCK_DRIVER_PROOF:
                return consTruckDriverProof;
            default:
                return null;
        }
    }
}
