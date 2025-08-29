package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
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
import com.dpw.runner.shipment.services.migration.repository.ICustomerBookingBackupRepository;
import com.dpw.runner.shipment.services.migration.service.interfaces.ICustomerBookingV3MigrationService;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.service.impl.ConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.v3.CustomerBookingV3Util;
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

@Service
@Slf4j
@Generated
public class CustomerBookingMigrationV3Service implements ICustomerBookingV3MigrationService {
    @Autowired
    ICustomerBookingDao customerBookingDao;

    @Autowired
    ICustomerBookingRepository customerBookingRepository;

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
    V1ServiceImpl v1Service;

    @Autowired
    HelperExecutor trxExecutor;

    @Autowired
    MigrationUtil migrationUtil;

    @Autowired
    CommonUtils commonUtils;

    @Autowired
    ConsolidationV3Service consolidationService;

    @Autowired
    private ICustomerBookingBackupRepository customerBookingBackupRepository;

    @Autowired
    private IPackingRepository packingRepository;

    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private CustomerBookingV3Util customerBookingV3Util;

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
        List<Long> bookingIds = fetchBookingFromDB(List.of(MigrationStatus.MIGRATED_FROM_V3.name(), MigrationStatus.CREATED_IN_V2.name()), tenantId);
        map.put("Total Bookings", bookingIds.size());
        List<Future<Long>> bookingQueue = new ArrayList<>();
        log.info("fetched {} bookingIds for Migrations", bookingIds.size());
        Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
        bookingIds.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    return trxExecutor.runInTrx(() -> {
                        try {

                            log.info("Migrating Customer Booking [id={}] and start time: {}", booking, System.currentTimeMillis());
                            CustomerBooking response = migrateBookingV2ToV3(booking, codeTeuMap);
                            log.info("Successfully migrated Customer Booking [oldId={}, newId={}] and end time: {}", booking, response.getId(), System.currentTimeMillis());
                            return response.getId();
                        } catch (Exception e) {
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during Customer Booking setup [id={}], exception: {}", booking, e.getLocalizedMessage());
                    migrationUtil.saveErrorResponse(booking, CUSTOMER_BOOKING, IntegrationType.V2_TO_V3_DATA_SYNC, Status.FAILED, e.getLocalizedMessage());

                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            bookingQueue.add(future);
        });

        List<Long> bookingsProcessed = collectAllProcessedIds(bookingQueue);
        map.put("Total Bookings Migrated", bookingsProcessed.size());
        log.info("Booking migration complete: {}/{} migrated for tenant [{}]", bookingsProcessed.size(), bookingIds.size(), tenantId);
        return map;
    }

    @Override
    public Map<String, Integer> migrateBookingV3ToV2ForTenant(Integer tenantId) {
        Map<String, Integer> map = new HashMap<>();
        List<Long> bookingIds = fetchBookingFromDB(List.of(MigrationStatus.MIGRATED_FROM_V2.name(), MigrationStatus.CREATED_IN_V3.name()), tenantId);
        map.put("Total Bookings", bookingIds.size());
        List<Future<Long>> bookingQueue = new ArrayList<>();
        log.info("fetched {} bookings for Migrations", bookingIds.size());
        Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
        bookingIds.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    return trxExecutor.runInTrx(() -> {
                        try {
                            log.info("Migrating v3 to v2 Customer Booking [id={}] and start time: {}", booking, System.currentTimeMillis());
                            CustomerBooking response = migrateBookingV3ToV2(booking, codeTeuMap);
                            log.info("Successfully migrated Customer Booking [oldId={}, newId={}] and end time: {}", booking, response.getId(), System.currentTimeMillis());
                            return response.getId();
                        } catch (Exception e) {
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during Customer Booking reverse migration [id={}], exception: {}", booking, e.getLocalizedMessage());
                    migrationUtil.saveErrorResponse(booking, CUSTOMER_BOOKING, IntegrationType.V3_TO_V2_DATA_SYNC, Status.FAILED, e.getLocalizedMessage());
                    customerBookingBackupRepository.deleteBackupByTenantIdAndBookingId(booking, tenantId);
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            bookingQueue.add(future);
        });

        List<Long> bookingsProcessed = collectAllProcessedIds(bookingQueue);
        map.put("Total Bookings Migrated", bookingsProcessed.size());
        log.info("Booking migration complete: {}/{} migrated for tenant [{}]", bookingsProcessed.size(), bookingIds.size(), tenantId);
        return map;
    }

    @Override
    public CustomerBooking migrateBookingV2ToV3(Long bookingId, Map<String, BigDecimal> codeTeuMap) throws RunnerException {
        Optional<CustomerBooking> customerBooking1 = customerBookingDao.findById(bookingId);
        if(customerBooking1.isEmpty()) {
            throw new DataRetrievalFailureException("No Booking found with given id: " + bookingId);
        }
        CustomerBooking booking = jsonHelper.convertValue(customerBooking1.get(), CustomerBooking.class);
        mapBookingV2ToV3(booking, codeTeuMap);
        customerBookingRepository.save(booking);
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
        customerBookingV3Util.updateCargoInformation(customerBooking, codeTeuMap, null, true);
        customerBooking.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);

        return null;
    }

    @Override
    public CustomerBooking migrateBookingV3ToV2(Long bookingId, Map<String, BigDecimal> codeTeuMap) {
        Optional<CustomerBooking> customerBooking1 = customerBookingDao.findById(bookingId);
        if(customerBooking1.isEmpty()) {
            throw new DataRetrievalFailureException("No Booking found with given id: " + bookingId);
        }
        CustomerBooking booking = jsonHelper.convertValue(customerBooking1.get(), CustomerBooking.class);
        mapBookingV3ToV2(booking, codeTeuMap);
        customerBookingRepository.save(booking);
        return booking;
    }

    @Override
    public CustomerBooking mapBookingV3ToV2(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) {
        updateContainerDataFromV3ToV2(customerBooking);
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
        containerRepository.saveAll(containersList);
    }

    private void updateContainerDataFromV3ToV2(CustomerBooking customerBooking) {
        List<Containers> containersList = customerBooking.getContainersList();
        for(Containers containers: containersList) {
            if(!Objects.isNull(containers.getGrossWeight())) {
                containers.setGrossWeight(containers.getGrossWeight().divide(new BigDecimal(containers.getContainerCount()), 3, RoundingMode.HALF_UP));
                containers.setCargoWeightPerContainer(containers.getGrossWeight());
            }
            if(!Objects.isNull(containers.getGrossWeightUnit())) {
                containers.setContainerWeightUnit(containers.getGrossWeightUnit());
            }
        }
        containerRepository.saveAll(containersList);
    }

    private void updatePackingDataFromV2ToV3(CustomerBooking customerBooking) throws RunnerException {
        List<Packing> packingList = packingDao.findByBookingIdIn(List.of(customerBooking.getId()));

        if (!packingList.isEmpty()) {
            for (Packing packing : packingList) {
                updateWeight(packing);
                updateVolume(packing);
                updateUnits(packing);
            }
        } else if (customerBooking.getIsPackageManual().equals(Boolean.TRUE)){
            Packing packing = createPackingFromBooking(customerBooking);
            packingList.add(packing);
            customerBooking.setIsPackageManual(Boolean.FALSE);
        }

        packingRepository.saveAll(packingList);
    }

    private void updateWeight(Packing packing) {
        if (packing.getWeight() != null) {
            packing.setCargoWeightPerPack(packing.getWeight());
            BigDecimal totalWeight = BigDecimal.valueOf(Long.parseLong(packing.getPacks())).multiply(packing.getCargoWeightPerPack());
            packing.setWeight(totalWeight);
        }
    }

    private void updateVolume(Packing packing) {
        if (isDimensionsPresent(packing)) {
            BigDecimal volumePerPack = packing.getLength().multiply(packing.getWidth()).multiply(packing.getHeight());
            packing.setVolumePerPack(volumePerPack);
            packing.setVolume(BigDecimal.valueOf(Long.parseLong(packing.getPacks())).multiply(volumePerPack));
        } else if (packing.getVolume() != null) {
            packing.setVolumePerPack(packing.getVolume());
            packing.setVolume(BigDecimal.valueOf(Long.parseLong(packing.getPacks())).multiply(packing.getVolumePerPack()));
        }
    }

    private void updateUnits(Packing packing) {
        if (packing.getWeightUnit() != null) {
            packing.setPackWeightUnit(packing.getWeightUnit());
        }
        if (packing.getVolumeUnit() != null) {
            packing.setVolumePerPackUnit(packing.getVolumeUnit());
        }
    }

    private Packing createPackingFromBooking(CustomerBooking booking) throws RunnerException {
        Packing packing = new Packing();
        if(!Objects.isNull(booking.getQuantity())) {
            packing.setPacks(String.valueOf(booking.getQuantity()));
        }
        packing.setPacksType(booking.getQuantityUnit());

        BigDecimal weight = Optional.ofNullable(booking.getGrossWeight()).orElse(BigDecimal.ZERO);
        String weightUnit = Optional.ofNullable(booking.getGrossWeightUnit()).orElse(WEIGHT_UNIT_KG);
        packing.setWeight(booking.getGrossWeight());
        packing.setWeightUnit(booking.getGrossWeightUnit());
        if(!Objects.isNull(booking.getQuantity())) {
            packing.setCargoWeightPerPack(weight.divide(BigDecimal.valueOf(booking.getQuantity()), 3, RoundingMode.HALF_UP));
        }
        packing.setPackWeightUnit(booking.getGrossWeightUnit());

        BigDecimal volume = Optional.ofNullable(booking.getVolume()).orElse(BigDecimal.ZERO);
        String volumeUnit = Optional.ofNullable(booking.getVolumeUnit()).orElse(VOLUME_UNIT_M3);
        packing.setVolume(booking.getVolume());
        packing.setVolumeUnit(booking.getVolumeUnit());
        if(!Objects.isNull(booking.getQuantity())) {
            packing.setVolumePerPack(volume.divide(BigDecimal.valueOf(booking.getQuantity()), 3, RoundingMode.HALF_UP));
        }
        packing.setVolumePerPackUnit(booking.getVolumeUnit());
        VolumeWeightChargeable vwOb = consolidationV3Service.calculateVolumeWeight(booking.getTransportType(), weightUnit, volumeUnit, weight, volume);

        packing.setChargeable(vwOb.getChargeable());
        packing.setChargeableUnit(vwOb.getChargeableUnit());
        packing.setTenantId(booking.getTenantId());
        packing.setCommodityGroup("FAK");
        packing.setBookingId(booking.getId());
        return packing;
    }

    private boolean isDimensionsPresent(Packing packing) {
        return packing.getLength()!= null && packing.getWidth()!= null && packing.getHeight()!= null;
    }

    private void updatePackingDataFromV3ToV2(CustomerBooking customerBooking) {
        List<Packing> packingList = customerBooking.getPackingList();
        for(Packing packing: packingList) {
            if(!Objects.isNull(packing.getWeight())) {
                packing.setWeight(packing.getWeight().divide(new BigDecimal(packing.getPacks()), 3, RoundingMode.HALF_UP));
                packing.setCargoWeightPerPack(packing.getWeight());
            }
            if(!Objects.isNull(packing.getVolume())) {
                packing.setVolume(packing.getVolume().divide(new BigDecimal(packing.getPacks()), 3, RoundingMode.HALF_UP));
                packing.setVolumePerPack(packing.getVolume());
            }
            if(!Objects.isNull(packing.getWeightUnit())) {
                packing.setPackWeightUnit(packing.getWeightUnit());
            }
            if(!Objects.isNull(packing.getVolumeUnit())) {
                packing.setVolumePerPackUnit(packing.getVolumeUnit());
            }
        }
        packingRepository.saveAll(packingList);
    }

    private Map<String, BigDecimal> getCodeTeuMapping() {
        try {
            v1Service.setAuthContext();
            DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
            List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(mdmResponse.getData(), MdmContainerTypeResponse.class);
            return containerTypes.stream()
                    .collect(Collectors.toMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
        } catch (RunnerException ex) {
            throw new MdmException(ex.getMessage());
        }
    }

    private List<Long> fetchBookingFromDB(List<String> migrationStatuses, Integer tenantId) {
        return customerBookingDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }
}
