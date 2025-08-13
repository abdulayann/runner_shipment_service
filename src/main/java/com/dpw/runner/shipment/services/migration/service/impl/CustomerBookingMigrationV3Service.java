package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
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
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.service.impl.ConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.dpw.runner.shipment.services.utils.CommonUtils;
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
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

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
        bookingIds.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
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
        bookingIds.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
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
        updateCargoInformation(customerBooking, codeTeuMap, null);
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
        containerDao.saveAll(containersList);
    }

    private void updateContainerDataFromV3ToV2(CustomerBooking customerBooking) {
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

        if (!packingList.isEmpty()) {
            for (Packing packing : packingList) {
                updateWeight(packing);
                updateVolume(packing);
                updateUnits(packing);
            }
        } else {
            Packing packing = createPackingFromBooking(customerBooking);
            if (packing != null) {
                packingList.add(packing);
            }
        }

        packingDao.saveAll(packingList);
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
        if (booking.getQuantity() == null || booking.getQuantity() == 0) return null;

        Packing packing = new Packing();
        packing.setPacks(String.valueOf(booking.getQuantity()));
        packing.setPacksType(booking.getQuantityUnit());

        BigDecimal quantity = BigDecimal.valueOf(booking.getQuantity());

        BigDecimal weight = Optional.ofNullable(booking.getGrossWeight()).orElse(BigDecimal.ZERO);
        String weightUnit = Optional.ofNullable(booking.getGrossWeightUnit()).orElse(WEIGHT_UNIT_KG);
        packing.setWeight(weight);
        packing.setWeightUnit(weightUnit);
        packing.setCargoWeightPerPack(weight.divide(quantity, RoundingMode.HALF_UP));
        packing.setPackWeightUnit(weightUnit);

        BigDecimal volume = Optional.ofNullable(booking.getVolume()).orElse(BigDecimal.ZERO);
        String volumeUnit = Optional.ofNullable(booking.getVolumeUnit()).orElse(VOLUME_UNIT_M3);
        packing.setVolume(volume);
        packing.setVolumeUnit(volumeUnit);
        packing.setVolumePerPack(volume.divide(quantity, RoundingMode.HALF_UP));
        packing.setVolumePerPackUnit(volumeUnit);

        VolumeWeightChargeable vwOb = consolidationV3Service.calculateVolumeWeight(
                booking.getTransportType(), weightUnit, volumeUnit, weight, volume);

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

    private List<Long> fetchBookingFromDB(List<String> migrationStatuses, Integer tenantId) {
        return customerBookingDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }

    public void updateCargoInformation(CustomerBooking booking, Map<String, BigDecimal> codeTeuMap, CustomerBooking oldBooking) throws RunnerException {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        List<Containers> containers = new ArrayList<>();
        List<Packing> packings = new ArrayList<>();
        if(booking.getId() != null) {
            containers = containerDao.findByBookingIdIn(List.of(booking.getId()));
            packings = packingDao.findByBookingIdIn(List.of(booking.getId()));
        }
        if (containers.isEmpty() && packings.isEmpty()) {
            customerBookingDao.save(booking);
            return;
        }
        if(containers.isEmpty()) {
            booking.setContainers(null);
            booking.setTeuCount(null);
        }
        else {
            updateContainersAndTeuInBooking(containers, codeTeuMap, booking);
        }
        updatePackagesInBookingCargoSummary(containers, packings, booking);
        updateWeightInBookingCargoSummary(containers, packings, booking, shipmentSettingsDetails);
        updateVolumeInBookingCargoSummary(packings, booking);
        calculateVW(booking, oldBooking);
        customerBookingDao.save(booking);
    }

    private void updateVolumeInBookingCargoSummary(List<Packing> packingList, CustomerBooking booking) throws RunnerException {
        BigDecimal totalVolume = BigDecimal.ZERO;
        List<String> volumeUnits = packingList.stream()
                .map(Packing::getVolumeUnit)
                .filter(Objects::nonNull)
                .toList();
        String volumeUnit = resolveUnit(volumeUnits, VOLUME_UNIT_M3);
        booking.setVolumeUnit(volumeUnit);
        if(!packingList.isEmpty()) {
            for (Packing packing : packingList) {
                totalVolume = addVolume(packing, totalVolume, volumeUnit);
            }
            booking.setVolume(totalVolume);
        }
    }

    private void updatePackagesInBookingCargoSummary(List<Containers> containersList, List<Packing> packingList, CustomerBooking booking) {
        List<String> containerPackUnits = containersList.stream()
                .map(Containers::getContainerPackageType)
                .filter(Objects::nonNull)
                .toList();
        String containerPackType = resolveUnit(containerPackUnits, PackingConstants.PKG);

        List<String> packagePackUnits = packingList.stream()
                .map(Packing::getPacksType)
                .filter(Objects::nonNull)
                .toList();
        String packagePackType = resolveUnit(packagePackUnits, PackingConstants.PKG);

        if(!packingList.isEmpty()) {
            Long totalPacks = 0L;
            for(Packing packing: packingList) {
                if (!isStringNullOrEmpty(packing.getPacks())) {
                    totalPacks += Long.parseLong(packing.getPacks());
                }
            }
            booking.setPackages(totalPacks);
            booking.setPackageType(packagePackType);
        } else if (!containersList.isEmpty()){
            Long totalPacks = getTotalContainerPackages(containersList);
            booking.setPackages(totalPacks);
            booking.setPackageType(containerPackType);
        }
        booking.setPackingList(packingList);
    }

    private void updateWeightInBookingCargoSummary(List<Containers> containersList, List<Packing> packingList, CustomerBooking booking, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        String defaultBranchWeightUnit = consolidationService.determineWeightChargeableUnit(shipmentSettingsDetails);
        String containerWeightUnit = resolveUnit(
                containersList.stream()
                        .map(Containers::getContainerWeightUnit)
                        .filter(Objects::nonNull)
                        .toList(),
                defaultBranchWeightUnit
        );

        String packingWeightUnit = resolveUnit(
                packingList.stream()
                        .map(Packing::getPackWeightUnit)
                        .filter(Objects::nonNull)
                        .toList(),
                defaultBranchWeightUnit
        );
        booking.setGrossWeightUnit(defaultBranchWeightUnit);
        if (!packingList.isEmpty()) {
            boolean ifAnyPackMissedWeight = false;
            for (Packing pack : packingList) {
                if (pack.getCargoWeightPerPack() == null) {
                    ifAnyPackMissedWeight = true;
                    break;
                }
            }

            if (!ifAnyPackMissedWeight) {
                booking.setGrossWeight(getTotalCargoWeightFromPackages(packingList, packingWeightUnit));
                booking.setGrossWeightUnit(packingWeightUnit);
            } else if(!containersList.isEmpty()) {
                booking.setGrossWeight(getTotalCargoWeight(containersList, containerWeightUnit));
                booking.setGrossWeightUnit(containerWeightUnit);
            }
        } else if(!containersList.isEmpty()) {
            booking.setGrossWeight(getTotalCargoWeight(containersList, containerWeightUnit));
            booking.setGrossWeightUnit(containerWeightUnit);
        }
    }

    private void updateContainersAndTeuInBooking(List<Containers> containersList, Map<String, BigDecimal> codeTeuMap, CustomerBooking booking) {
        for(Containers containers: containersList) {
            containers.setTeu(codeTeuMap.get(containers.getContainerCode()));
            Long containerCount = containers.getContainerCount();
            BigDecimal weightPerContainer = containers.getCargoWeightPerContainer();
            BigDecimal containerWeight = BigDecimal.valueOf(containerCount != null ? containerCount : 0).multiply(weightPerContainer != null ? weightPerContainer : BigDecimal.ZERO);
            if(Objects.isNull(containers.getGrossWeight())) {
                containers.setGrossWeight(containerWeight);
            }
            containers.setGrossWeightUnit(containers.getContainerWeightUnit());
        }
        BigDecimal teuCount = containersList.stream()
                .map(c -> codeTeuMap.getOrDefault(c.getContainerCode(), BigDecimal.ZERO)
                        .multiply(BigDecimal.valueOf(Optional.ofNullable(c.getContainerCount()).orElse(0L))))
                .reduce(BigDecimal.ZERO, BigDecimal::add)
                .setScale(1, RoundingMode.UNNECESSARY);
        booking.setTeuCount(teuCount);
        booking.setContainers(getTotalContainerCount(containersList));
        booking.setContainersList(containersList);
    }

    public BigDecimal getTotalCargoWeightFromPackages(List<Packing> packingList, String weightUnit) throws RunnerException {
        BigDecimal totalCargoWeight = BigDecimal.ZERO;
        for (Packing packing : packingList) {
            BigDecimal packingCount = packing.getPacks() != null ? new BigDecimal(packing.getPacks()) : BigDecimal.ZERO;
            BigDecimal weightPerPack = packing.getCargoWeightPerPack() != null ? packing.getCargoWeightPerPack() : BigDecimal.ZERO;
            BigDecimal totalLineCargoWeight = new BigDecimal(convertUnit(MASS, packingCount.multiply(weightPerPack), packing.getPackWeightUnit(), weightUnit).toString());

            totalCargoWeight = totalCargoWeight.add(totalLineCargoWeight);
        }
        return totalCargoWeight;
    }

    private Long getTotalContainerCount(List<Containers> containers) {
        return containers.stream().mapToLong(c -> c.getContainerCount() != null ? c.getContainerCount() : 0).sum();
    }

    public BigDecimal getTotalCargoWeight(List<Containers> containersList, String weightUnit) throws RunnerException {
        BigDecimal totalCargoWeight = BigDecimal.ZERO;
        for (Containers container : containersList) {
            BigDecimal containerCount = container.getContainerCount() != null ? new BigDecimal(container.getContainerCount()) : BigDecimal.ZERO;
            BigDecimal weightPerContainer = container.getCargoWeightPerContainer() != null ? container.getCargoWeightPerContainer() : BigDecimal.ZERO;
            BigDecimal totalLineCargoWeight = new BigDecimal(convertUnit(MASS, containerCount.multiply(weightPerContainer), container.getGrossWeightUnit(), weightUnit).toString());

            totalCargoWeight = totalCargoWeight.add(totalLineCargoWeight);
        }
        return totalCargoWeight;
    }

    public Long getTotalContainerPackages(List<Containers> containersList) {
        long totalLinePackages;
        long totalCargoSummaryPackages = 0L;
        for(Containers container: containersList) {
            long containerCount = container.getContainerCount() != null ? container.getContainerCount() : 0L;
            long packagesPerContainer = container.getPackagesPerContainer() != null ? container.getPackagesPerContainer() : 0L;
            totalLinePackages = containerCount * packagesPerContainer;

            totalCargoSummaryPackages += totalLinePackages;
        }
        return totalCargoSummaryPackages;
    }

    public String resolveUnit(List<String> unitsFromData, String branchDefaultUnit) {
        Set<String> distinctUnits = unitsFromData.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());

        return (distinctUnits.size() == 1) ? distinctUnits.iterator().next() : branchDefaultUnit;
    }

    private BigDecimal addVolume(Packing p, BigDecimal totalVolume, String volumeUnit) throws RunnerException {
        if (p.getVolume() != null && !isStringNullOrEmpty(p.getVolumeUnit())) {
            BigDecimal converted = new BigDecimal(convertUnit(VOLUME, p.getVolume(), p.getVolumeUnit(), volumeUnit).toString());
            return totalVolume.add(converted);
        }
        return totalVolume;
    }

    private void calculateVW(CustomerBooking customerBooking, CustomerBooking oldCustomerBooking) throws RunnerException {
        if (isStringNullOrEmpty(customerBooking.getTransportType()))
            return;
        boolean weightOrVolumeUpdated = isWeightOrVolumeUpdated(customerBooking, oldCustomerBooking);
        if (!isStringNullOrEmpty(customerBooking.getGrossWeightUnit()) && !isStringNullOrEmpty(customerBooking.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(customerBooking.getTransportType(), customerBooking.getGrossWeightUnit(), customerBooking.getVolumeUnit(), customerBooking.getGrossWeight(), customerBooking.getVolume());
            if (weightOrVolumeUpdated) {
                BigDecimal chargeable = vwOb.getChargeable();
                if (Constants.TRANSPORT_MODE_AIR.equals(customerBooking.getTransportType())) {
                    chargeable = BigDecimal.valueOf(roundOffAirShipment(chargeable.doubleValue()));
                }
                // LCL Sea transport special case
                if (Constants.TRANSPORT_MODE_SEA.equals(customerBooking.getTransportType()) && !isStringNullOrEmpty(customerBooking.getCargoType()) && Constants.SHIPMENT_TYPE_LCL.equals(customerBooking.getCargoType())) {
                    double volInM3 = convertUnit(Constants.VOLUME, customerBooking.getVolume(), customerBooking.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                    double wtInKg = convertUnit(Constants.MASS, customerBooking.getGrossWeight(), customerBooking.getGrossWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                    chargeable = BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3));
                    customerBooking.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                    vwOb = consolidationService.calculateVolumeWeight(customerBooking.getTransportType(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
                } else {
                    customerBooking.setChargeableUnit(vwOb.getChargeableUnit());
                }
                customerBooking.setChargeable(chargeable);
            }
            customerBooking.setWeightVolume(vwOb.getVolumeWeight());
            customerBooking.setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        }
    }

    private boolean isWeightOrVolumeUpdated(CustomerBooking newBooking, CustomerBooking oldBooking) {
        if (oldBooking == null) {
            return true;
        }
        return !newBooking.getGrossWeight().equals(oldBooking.getGrossWeight()) || !newBooking.getVolume().equals(oldBooking.getVolume());
    }

    private double roundOffAirShipment(double charge) {
        return (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) ?
                Math.floor(charge) + 0.5 : Math.ceil(charge);
    }
}
