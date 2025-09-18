package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CommonPackages;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

public interface ICommonPackageRepository extends MultiTenancyRepository<CommonPackages> {
    @Query(value = "select * " +
            "from common_packages where packing_ref_guid = ?1 and is_deleted is false;", nativeQuery = true)
    public CommonPackages getByGuid(String guid);

    @Query(value = "select * from common_packages where packing_ref_guid in (?1);", nativeQuery = true)
    public List<CommonPackages> findAllByGuid(List<UUID> guids);
}
