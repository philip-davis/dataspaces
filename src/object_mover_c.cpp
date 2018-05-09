#include "object_mover.hpp"

ObjectMover *om;

extern "C" {

  void sirius_ceph_initialize(const char* ceph_conf_file) {
    om = new ObjectMover(std::string(ceph_conf_file));
  }

  void sirius_ceph_create_async(int tier, const char *oid, const char *buf, size_t len, int *err) {
    std::string object_name(oid);
    librados::bufferlist bl;
    bl.append(buf, len);
    om->CreateAsync(static_cast<ObjectMover::Tier>(tier), object_name, bl, err);
  }

  void sirius_ceph_move_async(int tier, const char *oid, int *err) {
    std::string object_name(oid);
    om->MoveAsync(static_cast<ObjectMover::Tier>(tier), object_name, err);
  }

  void sirius_ceph_read_async(const char *oid, char* buf, uint64_t off, size_t len, int *ret) {
    std::string object_name(oid);
    om->CReadAsync(object_name, buf, off, len, ret);
  }

  void sirius_ceph_delete_async(const char *oid, int *err) {
    std::string object_name(oid);
    om->DeleteAsync(object_name, err);
  }

  void sirius_ceph_finalize() {
    delete om;
  }

}