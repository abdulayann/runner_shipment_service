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
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.exception.exceptions.MdmException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.ICustomerBookingV3MigrationService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
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
    V1ServiceImpl v1Service;

    @Autowired
    HelperExecutor trxExecutor;

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

        //Update CargoSummary
        updateCargoSummaryInBooking(customerBooking, codeTeuMap);

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
        customerBooking.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V3);
        return null;
    }

    private void updateCargoSummaryInBooking(CustomerBooking booking, Map<String, BigDecimal> codeTeuMap) throws RunnerException {
        List<Containers> containers = new ArrayList<>();
        List<Packing> packings = new ArrayList<>();
        if(booking.getId() != null) {
            containers = containerDao.findByBookingIdIn(List.of(booking.getId()));
            packings = packingDao.findByBookingIdIn(List.of(booking.getId()));
        }
        booking.setContainers(null);
        booking.setTeuCount(null);
        if (!containers.isEmpty()) {
            updateContainerInBooking(booking, codeTeuMap);
        }
        if (!packings.isEmpty()) {
            calculateCargoDetails(packings, booking);
        }
        calculateVW(booking);
    }

    public void calculateCargoDetails(List<Packing> packings, CustomerBooking customerBooking) throws RunnerException {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        int totalPacks = 0;
        Set<String> distinctPackTypes = new HashSet<>();
        boolean isAirTransport = Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(customerBooking.getTransportType());
        boolean stopWeightCalculation = false;

        for (Packing packing : packings) {
            totalVolume = addVolume(packing, totalVolume, customerBooking);
            if (!isStringNullOrEmpty(packing.getPacks())) {
                totalPacks += Integer.parseInt(packing.getPacks());
            }
            addDistinctPackType(distinctPackTypes, packing);
            if (!stopWeightCalculation) {
                boolean hasWeight = hasWeightInfo(packing);
                if (isAirTransport && !hasWeight) {
                    stopWeightCalculation = true;
                    continue;
                }
                BigDecimal weight = hasWeight ? new BigDecimal(convertUnit(MASS, packing.getWeight(), packing.getWeightUnit(), customerBooking.getGrossWeightUnit()).toString()) : BigDecimal.ZERO;
                totalWeight = totalWeight.add(weight);
            }
        }
        customerBooking.setGrossWeight(totalWeight);
        customerBooking.setVolume(totalVolume);
        customerBooking.setPackages((long) totalPacks);
        customerBooking.setGrossWeightUnit(Constants.WEIGHT_UNIT_KG);
        customerBooking.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        customerBooking.setPackageType(getPackUnit(distinctPackTypes));
    }

    private BigDecimal addVolume(Packing p, BigDecimal totalVolume, CustomerBooking booking) throws RunnerException {
        if (p.getVolume() != null && !isStringNullOrEmpty(p.getVolumeUnit())) {
            BigDecimal converted = new BigDecimal(convertUnit(VOLUME, p.getVolume(), p.getVolumeUnit(), booking.getVolumeUnit()).toString());
            return totalVolume.add(converted);
        }
        return totalVolume;
    }

    private boolean hasWeightInfo(Packing packing) {
        return packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit());
    }

    private void addDistinctPackType(Set<String> distinctPackTypes, Packing packing) {
        if (!isStringNullOrEmpty(packing.getPacksType())) {
            distinctPackTypes.add(packing.getPacksType());
        }
    }

    private String getPackUnit(Set<String> packTypes) {
        return (packTypes.size() == 1) ? packTypes.iterator().next() : PACKAGES;
    }

    private void updateContainerInBooking(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) {
        List<Containers> containersList = customerBooking.getContainersList();
        for(Containers containers: containersList) {
            containers.setTeu(codeTeuMap.get(containers.getContainerCode()));
        }
        customerBooking.setContainers(getTotalContainerCount(customerBooking.getContainersList()));
        customerBooking.setTeuCount(getTotalTeu(containersList, codeTeuMap));
    }

    public Map<String, BigDecimal> getCodeTeuMapping() {
        try {
            DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
            List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(mdmResponse.getData(), MdmContainerTypeResponse.class);
            return containerTypes.stream()
                    .collect(Collectors.toMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
        } catch (RunnerException ex) {
            throw new MdmException(ex.getMessage());
        }
    }

    private Long getTotalContainerCount(List<Containers> containers) {
        return containers.stream().mapToLong(c -> c.getContainerCount() != null ? c.getContainerCount() : 0).sum();
    }

    private BigDecimal getTotalTeu(List<Containers> containers, Map<String, BigDecimal> teuMap) {
        return containers.stream()
                .map(c -> teuMap.getOrDefault(c.getContainerCode(), BigDecimal.ZERO)
                        .multiply(BigDecimal.valueOf(Optional.ofNullable(c.getContainerCount()).orElse(0L))))
                .reduce(BigDecimal.ZERO, BigDecimal::add)
                .setScale(1, RoundingMode.UNNECESSARY);
    }

    private void calculateVW(CustomerBooking customerBooking) throws RunnerException {
        if (isStringNullOrEmpty(customerBooking.getTransportType()))
            return;
        if (!isStringNullOrEmpty(customerBooking.getGrossWeightUnit()) && !isStringNullOrEmpty(customerBooking.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationV3Service.calculateVolumeWeight(customerBooking.getTransportType(), customerBooking.getGrossWeightUnit(), customerBooking.getVolumeUnit(), customerBooking.getGrossWeight(), customerBooking.getVolume());
            customerBooking.setChargeable(vwOb.getChargeable());
            if (Constants.TRANSPORT_MODE_AIR.equals(customerBooking.getTransportType())) {
                customerBooking.setChargeable(BigDecimal.valueOf(roundOffAirShipment(customerBooking.getChargeable().doubleValue())));
            }
            customerBooking.setChargeableUnit(vwOb.getChargeableUnit());
            if (Constants.TRANSPORT_MODE_SEA.equals(customerBooking.getTransportType()) && !isStringNullOrEmpty(customerBooking.getCargoType()) && Constants.SHIPMENT_TYPE_LCL.equals(customerBooking.getCargoType())) {
                double volInM3 = convertUnit(VOLUME, customerBooking.getVolume(), customerBooking.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(MASS, customerBooking.getGrossWeight(), customerBooking.getGrossWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                customerBooking.setChargeable(BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3)));
                customerBooking.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                vwOb = consolidationV3Service.calculateVolumeWeight(customerBooking.getTransportType(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
            }

            customerBooking.setWeightVolume(vwOb.getVolumeWeight());
            customerBooking.setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        }
    }

    private List<CustomerBooking> fetchBookingFromDB(List<String> migrationStatuses, Integer tenantId) {
        return customerBookingDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }

}
