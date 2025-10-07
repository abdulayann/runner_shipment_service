package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import org.springframework.data.jpa.repository.Query;
import java.util.List;
import java.util.UUID;

public interface ICommonContainerRepository extends MultiTenancyRepository<CommonContainers> {
    @Query(value = "select * " +
            "from common_containers where container_ref_guid = ?1 and is_deleted is false;", nativeQuery = true)
    public CommonContainers getByGuid(UUID guid);

    @Query(value = "select * from common_containers where container_ref_guid in (?1);", nativeQuery = true)
    public List<CommonContainers> findAllByGuid(List<UUID> guids);
}
