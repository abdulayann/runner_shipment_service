package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.requests.Pageable;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.repository.interfaces.*;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

@Service
public class ShipmentService implements IShipmentService {

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IBlDetailsDao blDetailsDao;
    @Autowired
    private IMeasurementDao measurementDao;
    @Autowired
    private ICarrierDao carrierDao;
    @Autowired
    private IPartiesDao partiesDao;

    private List<String> TRANSPORT_MODES = Arrays.asList("SEA", "ROAD", "RAIL", "AIR");
    private List<String> SHIPMENT_TYPE = Arrays.asList("FCL", "LCL");
    private List<String> WEIGHT_UNIT = Arrays.asList("KGS", "G", "DT");
    private List<String> VOLUME_UNIT = Arrays.asList("M3", "L3", "CC");
    private List<String> SHIPPING_LINE = Arrays.asList("DPWC", "MARUSK", "APLU");
    private List<String> LOCATIONS = Arrays.asList("Jabel Ali", "Nava Shiva", "Shanghai", "Vancouver", "Seattle");
    private List<String> PARTY_TYPE = Arrays.asList("CLIENT","CONSIGNER", "CONSIGNEE");
    private List<String> DIRECTIONS = Arrays.asList("IMP","EXP");
    private List<String> SOURCE = Arrays.asList("API","Runner", "Logistics");

    private Map<String, Object> ADDRESS = Map.ofEntries(
        Map.entry("AddressShortCode", "Default")
    );
    private Map<String, Object> ORG = Map.ofEntries(
            Map.entry("TenantName", "DP WORLD LOGISTICS CANADA INC")
    );

    @Override @Transactional
    public List<ShipmentDetails> createTestShipment(Integer count) {
        List<ShipmentDetails> response = new ArrayList<>();
        /**
         * BL details
         * Measurements
         * carrier
         * pickup
         * Delivery* *
         * Shipment details
         * Parties*
         * * * * * * *
         * * * */

        for (int i = 0; i < count; i++) {

            ShipmentDetails shipmentDetail = createShipmentData();
            /**
             * Bl Details*
             */
            BlDetails blDetail = createBlData();
            shipmentDetail.setBlDetails(blDetail);
            /**
             * Measurement Details*
             */
            MeasurementDetails measurementDetail = createMeasurement();
            shipmentDetail.setMeasurementDetails(measurementDetail);
            /**
             * Carrier Details*
             */
            CarrierDetails carrierDetail = createCarrier();
            shipmentDetail.setCarrierDetails(carrierDetail);
            /**
             * * TODO
             * Pickup Details*
             */
            PickupDetails pickupDetail = createPickup();
            /**
             * * TODO
             * Pickup Details*
             */
            DeliveryDetails deliveryDetail = createDelivery();

            shipmentDetail = shipmentDao.save(shipmentDetail);
            /**
             * Parties Details*
             */
            List<PartiesDetails> partiesDetails = createParties(shipmentDetail);
            shipmentDetail.setParties(partiesDetails);
            response.add(shipmentDetail);
        }

        return response;
    }

    @Override
    public RunnerResponse fetchShipments(Pageable pageable) {
        double start = System.currentTimeMillis();
        org.springframework.data.domain.Pageable pages;
        if(pageable.getSortRequest() != null && pageable.getFilterCriteria() != null && pageable.getFilterCriteria().size() == 0) {
            Sort sortRequest = Sort.by(IShipmentDao.tableNames.get(pageable.getSortRequest().getFieldName()) +"."+ pageable.getSortRequest().getFieldName());
            sortRequest = sortRequest.descending();
            pages = PageRequest.of(pageable.getPageNo(), pageable.getLimit(), sortRequest);
        } else {
            pages = PageRequest.of(pageable.getPageNo(), pageable.getLimit());
        }
        Page<ShipmentDetails> page  = shipmentDao.findAll(IShipmentDao.fetchShipmentData((pageable.getFilterCriteria() == null ? new ArrayList<>() : pageable.getFilterCriteria()), pageable.getSortRequest()),pages);
        System.out.println((System.currentTimeMillis() - start));
        RunnerResponse runnerResponse = RunnerResponse.builder().data(page.getContent()).pageNo(page.getTotalPages()).count(page.getTotalElements()).build();
        return runnerResponse;
    }

    private List<PartiesDetails> createParties(ShipmentDetails shipmentDetails) {
        List<PartiesDetails> parties = new ArrayList<>();
        int random = new Random().nextInt(100);
        for (String partyType: PARTY_TYPE) {
            PartiesDetails party = PartiesDetails.builder()
                        .guid(UUID.randomUUID()).type(partyType).orgId(random).addressId(random)
                        .orgData(ORG).addressData(ADDRESS)
                        .entityId(shipmentDetails.getId()).entityType("SHIPMENT")
                        .build();
            party.setTenantId(1);
            parties.add(party);
        }
        parties = partiesDao.saveAll(parties);
        return parties;
    }

    private DeliveryDetails createDelivery() {
        return null;
    }

    private PickupDetails createPickup() {
        return null;
    }

    private CarrierDetails createCarrier() {
        int random = new Random().nextInt(100);
        CarrierDetails carrier = CarrierDetails.builder()
                .guid(UUID.randomUUID()).shippingLine(SHIPPING_LINE.get(random % SHIPPING_LINE.size()))
                .vessel(generateString(5)).voyage(generateString(5)).origin(LOCATIONS.get(random % LOCATIONS.size())).destination(LOCATIONS.get(random % LOCATIONS.size()))
                .eta(LocalDateTime.now()).etd(LocalDateTime.now()).ata(LocalDateTime.now()).atd(LocalDateTime.now())
                .build();
        carrier.setTenantId(1);
        return carrierDao.save(carrier);
    }

    private MeasurementDetails createMeasurement() {
        int random = new Random().nextInt(100);
        MeasurementDetails measurementDetails = MeasurementDetails.builder().guid(UUID.randomUUID())
                .volume(new BigDecimal(random)).volumeUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size()))
                .volumetricWeight(new BigDecimal(random)).volumetricWeightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
                .chargable(new BigDecimal(random)).chargeableUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size()))
                .netWeight(new BigDecimal(random)).netWeightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
                .noOfPacks(random).packsUnit("BAG")
                .build();
        measurementDetails.setTenantId(1);
        return measurementDao.save(measurementDetails);
    }

    private BlDetails createBlData() {
        int random = new Random().nextInt(100);
        BlDetails blDetails = BlDetails.builder()
                .guid(UUID.randomUUID()).releaseType(generateString(3)).hblType(generateString(3))
                .deliveryMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).screeningStatus(generateString(3))
                .build();
        blDetails.setTenantId(1);
        return blDetailsDao.save(blDetails);
    }

    private ShipmentDetails createShipmentData() {
        int random = new Random().nextInt(100);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().guid(UUID.randomUUID()).direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(1)
                .source(SOURCE.get(random % SOURCE.size())).transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .houseBill(generateString(10)).masterBill(generateString(10)).bookingReference(generateString(10)).consolRef(generateString(10)).paymentTerms(generateString(3))
                .goodsDescription(generateString(10)).additionalTerms(generateString(10))
                .build();
        shipmentDetails.setTenantId(1);
        return shipmentDetails;
    }

    private String generateString(int length) {
        String SALTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
        StringBuilder salt = new StringBuilder();
        Random rnd = new Random();
        while (salt.length() < length) {
            int index = (int) (rnd.nextFloat() * SALTCHARS.length());
            salt.append(SALTCHARS.charAt(index));
        }
        return salt.toString();
    }


}
