package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.exception.exceptions.MdmException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.ICustomerBookingV3MigrationService;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingV3Service;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;
import java.math.RoundingMode;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.migration.utils.MigrationUtil.collectAllProcessedIds;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.roundOffAirShipment;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Service
@Slf4j
@Generated
public class CustomerBookingMigrationV3Service implements ICustomerBookingV3MigrationService {
    @Autowired
    ICustomerBookingDao customerBookingDao;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    MDMServiceAdapter mdmServiceAdapter;

    @Autowired
    IPackingDao packingDao;

    @Autowired
    IContainerDao containerDao;

    @Autowired
    IConsolidationV3Service consolidationV3Service;

    @Autowired
    ICustomerBookingV3Service customerBookingV3Service;

    @Autowired
    V1ServiceImpl v1Service;

    @Autowired
    HelperExecutor trxExecutor;

    @Autowired
    MigrationUtil migrationUtil;

    Map<String, String> v2ToV3ServiceTypeMap = Map.ofEntries(
            // AIR
            Map.entry("AIR_C2P", "C2P"),
            Map.entry("AIR_F2F", "F2F"),
            Map.entry("AIR_F2P", "F2A"),
            Map.entry("AIR_P2P", "A2A"),
            Map.entry("AIR_A2A", "A2A"),
            Map.entry("AIR_F2A", "F2A"),
            Map.entry("AIR_A2F", "A2F"),
            Map.entry("AIR_C2C", "C2C"),
            Map.entry("AIR_C2F", "C2F"),
            Map.entry("AIR_F2C", "F2C"),
            Map.entry("AIR_P2C", "P2C"),
            Map.entry("AIR_P2F", "A2F"),

            // SEA
            Map.entry("SEA_C2P", "C2P"),
            Map.entry("SEA_F2F", "F2F"),
            Map.entry("SEA_F2P", "F2P"),
            Map.entry("SEA_P2P", "P2P"),
            Map.entry("SEA_A2A", "P2P"),
            Map.entry("SEA_F2A", "F2P"),
            Map.entry("SEA_A2F", "P2F"),
            Map.entry("SEA_C2C", "C2C"),
            Map.entry("SEA_C2F", "C2F"),
            Map.entry("SEA_F2C", "F2C"),
            Map.entry("SEA_P2C", "P2C"),
            Map.entry("SEA_P2F", "P2F"),

            // RAIL
            Map.entry("RAIL_C2P", "C2P"),
            Map.entry("RAIL_F2F", "F2F"),
            Map.entry("RAIL_F2P", "F2P"),
            Map.entry("RAIL_P2P", "P2P"),
            Map.entry("RAIL_A2A", "P2P"),
            Map.entry("RAIL_F2A", "F2P"),
            Map.entry("RAIL_A2F", "P2F"),
            Map.entry("RAIL_C2C", "C2C"),
            Map.entry("RAIL_C2F", "C2F"),
            Map.entry("RAIL_F2C", "F2C"),
            Map.entry("RAIL_P2C", "P2C"),
            Map.entry("RAIL_P2F", "P2F"),

            // ROAD
            Map.entry("ROAD_C2P", "C2P"),
            Map.entry("ROAD_F2F", "F2F"),
            Map.entry("ROAD_F2P", "F2P"),
            Map.entry("ROAD_P2P", "P2P"),
            Map.entry("ROAD_A2A", "P2P"),
            Map.entry("ROAD_F2A", "F2P"),
            Map.entry("ROAD_A2F", "P2F"),
            Map.entry("ROAD_C2C", "C2C"),
            Map.entry("ROAD_C2F", "C2F"),
            Map.entry("ROAD_F2C", "F2C"),
            Map.entry("ROAD_P2C", "P2C"),
            Map.entry("ROAD_P2F", "P2F")
    );


    @Override
    public Map<String, Integer> migrateBookingV2ToV3ForTenant(Integer tenantId) {
        Map<String, Integer> map = new HashMap<>();
        List<CustomerBooking> customerBookingList = fetchBookingFromDB(List.of(MigrationStatus.MIGRATED_FROM_V3.name(), MigrationStatus.CREATED_IN_V2.name()), tenantId);
        map.put("Total Bookings", customerBookingList.size());
        List<Future<Long>> bookingQueue = new ArrayList<>();
        log.info("fetched {} bookings for Migrations", customerBookingList.size());
        customerBookingList.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
                    return trxExecutor.runInTrx(() -> {
                        try {

                            CustomerBooking response = migrateBookingV2ToV3(booking, codeTeuMap);
                            return response.getId();
                        } catch (Exception e) {
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            bookingQueue.add(future);
        });

        List<Long> bookingsProcessed = collectAllProcessedIds(bookingQueue);
        map.put("Total Bookings Migrated", bookingsProcessed.size());
        log.info("Booking migration complete: {}/{} migrated for tenant [{}]", bookingsProcessed.size(), customerBookingList.size(), tenantId);
        return map;
    }

    @Override
    public Map<String, Integer> migrateBookingV3ToV2ForTenant(Integer tenantId) {
        Map<String, Integer> map = new HashMap<>();
        List<CustomerBooking> customerBookingList = fetchBookingFromDB(List.of(MigrationStatus.MIGRATED_FROM_V2.name(), MigrationStatus.CREATED_IN_V3.name()), tenantId);
        map.put("Total Bookings", customerBookingList.size());
        List<Future<Long>> bookingQueue = new ArrayList<>();
        log.info("fetched {} bookings for Migrations", customerBookingList.size());
        customerBookingList.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
                    return trxExecutor.runInTrx(() -> {
                        try {

                            CustomerBooking response = migrateBookingV3ToV2(booking, codeTeuMap);
                            return response.getId();
                        } catch (Exception e) {
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    migrationUtil.saveErrorResponse(booking.getId(), CUSTOMER_BOOKING, IntegrationType.V3_TO_V2_DATA_SYNC, Status.FAILED, e.getLocalizedMessage());
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            bookingQueue.add(future);
        });

        List<Long> bookingsProcessed = collectAllProcessedIds(bookingQueue);
        map.put("Total Bookings Migrated", bookingsProcessed.size());
        log.info("Booking migration complete: {}/{} migrated for tenant [{}]", bookingsProcessed.size(), customerBookingList.size(), tenantId);
        return map;
    }

    @Override
    public CustomerBooking migrateBookingV2ToV3(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) throws RunnerException {
        Optional<CustomerBooking> customerBooking1 = customerBookingDao.findById(customerBooking.getId());
        if(customerBooking1.isEmpty()) {
            throw new DataRetrievalFailureException("No Booking found with given id: " + customerBooking.getId());
        }
        CustomerBooking booking = jsonHelper.convertValue(customerBooking1.get(), CustomerBooking.class);
        mapBookingV2ToV3(booking, codeTeuMap);
        customerBookingDao.save(booking);
        return booking;
    }

    @Override
    public CustomerBooking mapBookingV2ToV3(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) throws RunnerException {
        //update serviceType based on the transport type
        String transportMode = customerBooking.getTransportType();
        String serviceTypeV2 = customerBooking.getServiceMode();
        String v3Key = transportMode + "_" + serviceTypeV2;
        customerBooking.setServiceMode(v2ToV3ServiceTypeMap.getOrDefault(v3Key, serviceTypeV2));
        updateContainerDataFromV2ToV3(customerBooking, codeTeuMap);
        updatePackingDataFromV2ToV3(customerBooking);

        //Update CargoSummary
        customerBookingV3Service.updateCargoInformation(customerBooking, codeTeuMap, null);
        customerBooking.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);

        return null;
    }

    @Override
    public CustomerBooking migrateBookingV3ToV2(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) {
        Optional<CustomerBooking> customerBooking1 = customerBookingDao.findById(customerBooking.getId());
        if(customerBooking1.isEmpty()) {
            throw new DataRetrievalFailureException("No Booking found with given id: " + customerBooking.getId());
        }
        CustomerBooking booking = jsonHelper.convertValue(customerBooking1.get(), CustomerBooking.class);
        mapBookingV3ToV2(booking, codeTeuMap);
        customerBookingDao.save(booking);
        return booking;
    }

    @Override
    public CustomerBooking mapBookingV3ToV2(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) {
        updateContainerDataFromV3ToV2(customerBooking, codeTeuMap);
        updatePackingDataFromV3ToV2(customerBooking);
        customerBooking.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V3);
        return null;
    }

    private void updateContainerDataFromV2ToV3(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) {
        List<Containers> containersList = containerDao.findByBookingIdIn(List.of(customerBooking.getId()));
        for(Containers containers: containersList) {
            if(!Objects.isNull(containers.getGrossWeight())) {
                containers.setCargoWeightPerContainer(containers.getGrossWeight());
                containers.setGrossWeight(new BigDecimal(containers.getContainerCount()).multiply(containers.getCargoWeightPerContainer()));
            }
            if(!Objects.isNull(containers.getGrossWeightUnit())) {
                containers.setContainerWeightUnit(containers.getGrossWeightUnit());
            }
            containers.setPackagesPerContainer(null);
            containers.setContainerPackageType(null);
            containers.setTeu(codeTeuMap.get(containers.getContainerCode()));
        }
        containerDao.saveAll(containersList);
    }

    private void updateContainerDataFromV3ToV2(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) {
        List<Containers> containersList = customerBooking.getContainersList();
        for(Containers containers: containersList) {
            if(!Objects.isNull(containers.getCargoWeightPerContainer())) {
                containers.setGrossWeight(containers.getCargoWeightPerContainer());
            }
            if(!Objects.isNull(containers.getGrossWeightUnit())) {
                containers.setGrossWeightUnit(containers.getContainerWeightUnit());
            }
        }
        containerDao.saveAll(containersList);
    }

    private void updatePackingDataFromV2ToV3(CustomerBooking customerBooking) throws RunnerException {
        List<Packing> packingList = packingDao.findByBookingIdIn(List.of(customerBooking.getId()));
        if(!packingList.isEmpty()) {
            for(Packing packing: packingList) {
                if(!Objects.isNull(packing.getWeight())) {
                    packing.setCargoWeightPerPack(packing.getWeight());
                    packing.setWeight(new BigDecimal(packing.getPacks()).multiply(packing.getCargoWeightPerPack()));
                }
                if(!Objects.isNull(packing.getVolume())) {
                    packing.setVolumePerPack(packing.getVolume());
                    packing.setVolume(new BigDecimal(packing.getPacks()).multiply(packing.getVolumePerPack()));
                }
                if(!Objects.isNull(packing.getWeightUnit())) {
                    packing.setPackWeightUnit(packing.getWeightUnit());
                }
                if(!Objects.isNull(packing.getVolumeUnit())) {
                    packing.setVolumePerPackUnit(packing.getVolumeUnit());
                }
            }
        } else {
            if(customerBooking.getQuantity() != null && !customerBooking.getQuantity().equals(0)) {
                Packing packing = new Packing();
                packing.setPacks(String.valueOf(customerBooking.getQuantity()));
                packing.setPacksType(customerBooking.getQuantityUnit());
                if(customerBooking.getGrossWeight() != null) {
                    packing.setWeight(customerBooking.getGrossWeight());
                    packing.setWeightUnit(customerBooking.getGrossWeightUnit());
                    packing.setCargoWeightPerPack(customerBooking.getGrossWeight().divide(new BigDecimal(customerBooking.getQuantity()), RoundingMode.UNNECESSARY));
                    packing.setPackWeightUnit(customerBooking.getGrossWeightUnit());
                } else {
                    packing.setWeight(BigDecimal.ZERO);
                    packing.setWeightUnit(WEIGHT_UNIT_KG);
                    packing.setCargoWeightPerPack(BigDecimal.ZERO);
                    packing.setPackWeightUnit(WEIGHT_UNIT_KG);
                }
                if(customerBooking.getVolume() != null) {
                    packing.setVolume(customerBooking.getVolume());
                    packing.setVolumeUnit(customerBooking.getVolumeUnit());
                    packing.setVolumePerPack(customerBooking.getVolume().divide(new BigDecimal(customerBooking.getQuantity()), RoundingMode.UNNECESSARY));
                    packing.setVolumePerPackUnit(customerBooking.getVolumeUnit());
                } else {
                    packing.setVolume(BigDecimal.ZERO);
                    packing.setVolumeUnit(VOLUME_UNIT_M3);
                    packing.setVolumePerPack(BigDecimal.ZERO);
                    packing.setVolumePerPackUnit(VOLUME_UNIT_M3);
                }
                VolumeWeightChargeable vwOb = consolidationV3Service.calculateVolumeWeight(customerBooking.getTransportType(), packing.getWeightUnit(), packing.getVolumeUnit(), packing.getWeight(), packing.getVolume());
                packing.setChargeable(vwOb.getChargeable());
                packing.setChargeableUnit(vwOb.getChargeableUnit());
                packing.setTenantId(customerBooking.getTenantId());
                packing.setCommodityGroup("FAK");
                packing.setBookingId(customerBooking.getId());
                packingList.add(packing);
            }
        }
        packingDao.saveAll(packingList);
    }

    private void updatePackingDataFromV3ToV2(CustomerBooking customerBooking) {
        List<Packing> packingList = customerBooking.getPackingList();
        for(Packing packing: packingList) {
            if(!Objects.isNull(packing.getCargoWeightPerPack())) {
                packing.setWeight(packing.getCargoWeightPerPack());
            }
            if(!Objects.isNull(packing.getVolumePerPack())) {
                packing.setVolume(packing.getVolumePerPack());
            }
        }
        packingDao.saveAll(packingList);
    }

    private Map<String, BigDecimal> getCodeTeuMapping() {
        try {
            DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
            List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(mdmResponse.getData(), MdmContainerTypeResponse.class);
            return containerTypes.stream()
                    .collect(Collectors.toMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
        } catch (RunnerException ex) {
            throw new MdmException(ex.getMessage());
        }
    }

    private List<CustomerBooking> fetchBookingFromDB(List<String> migrationStatuses, Integer tenantId) {
        return customerBookingDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }

}
